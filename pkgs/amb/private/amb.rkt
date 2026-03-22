#lang racket/base

;; Core implementation of the ambiguous operator.  This module
;; defines the runtime primitives used by the public API in
;; "amb/main.rkt" and its typed counterpart.

(require "utils.rkt"
         (for-syntax racket/base syntax/parse)
         racket/sequence
         racket/stream
         goto)

(provide empty-mutable-vector
         fail
         amb amb* unsafe-amb*
         for/amb for*/amb
         return-prompt-tag
         amb-prompt-tag
         in-amb  in-amb*
         in-amb/do in-amb*/do
         (struct-out exn:fail:contract:amb)
         raise-amb-error
         current-amb-prompt-tag
         current-amb-empty-handler
         current-amb-shuffler
         current-amb-rotator
         current-amb-maker
         current-amb-tasks
         current-amb-length
         current-amb-pusher
         current-amb-popper)

(define call/prompt call-with-continuation-prompt)
(define abort/cc abort-current-continuation)
(define return-prompt-tag (make-continuation-prompt-tag 'return))
(define (FALSE) #f)

(define (fail #:empty-handler [empty-handler (current-amb-empty-handler)]
              #:tasks [task* (current-amb-tasks)]
              #:length [length (current-amb-length)])
  ;; Jump to the next pending task or trigger the empty handler when
  ;; no choices remain.
  (when (zero? (length task*))
    (empty-handler))
  (goto (sequence-ref task* 0)))

(define (unsafe-amb* alt*)
  ;; Process a vector of alternative thunks, using the task queue to
  ;; implement backtracking.
  ;;
  ;; Control-flow overview
  ;; ---------------------
  ;; `unsafe-amb*` uses a labelled re-entry trick: `task` is a
  ;; continuation capturing the point just after `(define task ...)`,
  ;; so that jumping to it re-executes the body from the `case/eq`
  ;; dispatch onwards.  The mutable variable `pos` records where in
  ;; the iteration we are:
  ;;
  ;; - #t  -- first entry (normal call, not a backtrack jump)
  ;;   Register `task` at the front of the task queue so that a later
  ;;   `(amb)` / `fail` elsewhere can jump back here.  Then
  ;;   immediately jump to the front of the queue, which will either
  ;;   be `task` itself (if nothing else is queued) or some outer
  ;;   choice point that was registered earlier.
  ;; - 0,1,... -- index of the alternative currently being tried
  ;;   Pick the next alternative, advance `pos`, and call the thunk.
  ;;   If `pos` reaches `len` after the pick, clear `alt*` so the
  ;;   vector can be GC'd and mark `pos` as #f for the next
  ;;   re-entry.
  ;; - #f  -- all alternatives exhausted
  ;;   All choices are used up.  Remove `task` from the queue (it
  ;;   was already the front entry) and call `fail` to continue
  ;;   backtracking to the next outer choice point.
  (define len (vector-length alt*))
  (define length (current-amb-length))
  (define task* (current-amb-tasks))
  ((current-amb-shuffler) alt*)    ; allow user-provided randomization
  (when (zero? len)
    (fail #:tasks task*
          #:length length))

  (define first? #t)
  (define pos 0)
  ;; Capture the current continuation delimited by
  ;; `current-amb-prompt-tag`.  Every subsequent backtrack jump lands
  ;; here and re-runs the dispatch below.
  (define task (label (current-amb-prompt-tag)))
  (when first?
    ;; First entry: register this choice point and hand control to
    ;; whatever task is at the front of the queue.
    (set! first? #f)
    ((current-amb-pusher) task* task)
     (goto (sequence-ref task* 0)))
  (when (= pos len)
    ;; Re-entry: fall through to the alternative-dispatch code below.
    (set! pos #f)
    ((current-amb-popper) task*))
  (unless pos
    (fail #:tasks task*
          #:length length))

  ;; Pick the current alternative and advance the position for the
  ;; next re-entry.
  (define alt (vector-ref alt* pos))
  ;; Replace the slot with `amb*` as a sentinel so we can detect
  ;; skipped (placeholder) alternatives later.
  (vector-set! alt* pos amb*)
  (set! pos (add1 pos))
  (when (= pos len)
    ;; Once `pos` reaches the end we no longer need the vector `alt*`;
    ;; drop the reference so the thunks can be GC'd.
    (set! alt* empty-mutable-vector))
  (when (equal? alt amb*)
    ;; If the slot held the sentinel `amb*`, skip it and jump back to
    ;; pick the next one.
    (goto task))
  ;; Give the user-provided rotator a chance to reorder the queue
  ;; before running the chosen alternative.
  ((current-amb-rotator) task*)
  (alt))

(define amb*
  ;; Public-facing helper that accepts any number of thunks and
  ;; delegates to `unsafe-amb*` after packing them into a vector.
  (case-λ
    [() (unsafe-amb* empty-mutable-vector)]
    [(alt) (unsafe-amb* (vector alt))]
    [(alt1 alt2) (unsafe-amb* (vector alt1 alt2))]
    [(alt1 alt2 alt3) (unsafe-amb* (vector alt1 alt2 alt3))]
    [alt* (unsafe-amb* (list->vector alt*))]))

(define-syntax (amb stx)
  ;; Macro form for writing `(amb expr ...)`.  Each expression is
  ;; delayed, and the resulting alternatives are attempted in an
  ;; unspecified order.
  (syntax-parse stx
    [(_)
     (syntax/loc stx
       (unsafe-amb* empty-mutable-vector))]
    [(_ expr ...)
     (syntax/loc stx
       (unsafe-amb* (vector (λ () expr) ...)))]))


(define-syntaxes (for/amb for*/amb)
  ;; Variants of `for` that evaluate the body in an ambiguous
  ;; context.  Each clause behaves like its `for` equivalent
  ;; but backtracks on `(amb)`.
  (let ()
    (define-splicing-syntax-class break-clause
      [pattern (~seq (~or* #:break #:final) guard:expr)])
    (define ((make-for/amb for/vector for) stx)
      ;; Two expansion strategies are used depending on whether a
      ;; `#:length` clause is present.
      (syntax-parse stx
        ;; With `#:length n`
        ;;   A fixed-size vector of thunks is built up front (one per
        ;;   iteration) and handed directly to `unsafe-amb*`.  This is
        ;;   identical to writing `(amb expr0 expr1 ...)` by hand but
        ;;   works for any sequence whose length is known in advance.
        [(_ #:length n (~optional (~seq #:fill fill-expr))
            (clauses ...) break:break-clause ...
            body ...+)
         #:with fill
         (if (attribute fill-expr)
             #'(λ () fill-expr)
             #'amb*)
         (quasisyntax/loc stx
           (unsafe-amb*
            (let ([m n])
              (if (zero? m)
                  empty-mutable-vector
                  (#,for/vector
                   #:length m #:fill fill
                   (clauses ...) break ...
                   (define (alt) body ...)
                   alt)))))]
        ;; Without `#:length n` (the general case)
        ;;   The loop runs eagerly but each iteration is turned into a
        ;;   choice point on the fly, so the sequence may be infinite.
        [(_ (clauses ...) break:break-clause ... body ...+)
         (quasisyntax/loc stx
           (call/prompt
            (λ (task* length)
              ;; Control-flow overview
              ;; ---------------------
              ;; - `return-prompt-tag`:
              ;;   used to deliver a result to the caller without
              ;;   running the rest of the loop.
              ;; - `task`:
              ;;   continuation at the top of the `call/prompt` body;
              ;;   jumping here re-enters the for loop from the start,
              ;;   but `retry` tells us which iteration to resume.
              ;; - `retry`:
              ;;   starts as `#f` (first entry), then holds the
              ;;   continuation of the most recently started iteration
              ;;   (`choice`), and finally a sentinel (`skip`) once
              ;;   the loop has finished.
              (define first? #t)
              #;(: retry (∪ False (¬ False)))
              (define retry #f)
              ;; `task` is the re-entry point for this group of
              ;; choices.  Backtracking jumps here and then
              ;; `(goto retry #f)` forwards control to the specific
              ;; iteration that should run next.
              (define task (label (current-amb-prompt-tag)))
              (when retry
                ;; `retry` : `(¬ False)`
                ;; Re-entry: `retry` now holds the continuation of
                ;; the iteration to resume; jump directly there.
                (goto retry #f))
              (when first?
                ;; First entry: register `task` and yield to any
                ;; earlier choice point in the queue.
                (set! first? #f)
                ((current-amb-pusher) task* task)
                (goto (sequence-ref task* 0)))

              ;; `retry` : `False`
              (#,for (clauses ...) break ...
               ;; Capture the continuation of this iteration so
               ;; that backtracking can resume the loop here.
               #;(: choice (∪ False (¬ False)))
               (define choice (label return-prompt-tag))
               (when choice
                 ;; `choice` : `(¬ False)`
                 (define (alt) body ...)
                 (set! retry choice)
                 ((current-amb-rotator) task*)
                 ;; Deliver the body result to the caller, suspending
                 ;; the loop until the next backtrack.
                 (abort/cc return-prompt-tag alt)))

              ;; The loop has run all iterations.  Deregister `task`,
              ;; record the "done" point so a stale jump to `task`
              ;; followed by `(goto retry #f)` ends up here harmlessly,
              ;; and propagate failure outward.
              ((current-amb-popper) task*)
              #;(: skip (∪ False (¬ False)))
              (define skip (label return-prompt-tag))
              (when skip
                ;; `skip` : `(¬ False)`
                (set! retry skip))
              (fail #:tasks task*
                    #:length length))
            return-prompt-tag
            #f
            (current-amb-tasks)
            (current-amb-length)))]))
    (values (make-for/amb #'for/vector  #'for)
            (make-for/amb #'for*/vector #'for*))))


(define-values (in-amb* in-amb*/do)
  ;; Both `in-amb*` and `in-amb*/do` wrap the same coroutine
  ;; machinery; they differ only in whether they return a lazy stream
  ;; or a general sequence (via `make-do-sequence`).
  ;;
  ;; Coroutine channel
  ;; -----------------
  ;; `resume` and `cache` together implement a two-way channel
  ;; between the sequence consumer and the search engine.
  ;;
  ;; `resume` is a full continuation captured at the `in-amb*`
  ;; call site.  Jumping to it (via `(goto resume val)`) re-enters
  ;; the search context and delivers `val` as the result of the
  ;; `(label)` expression inside `continue-with-pos?`.
  ;;
  ;; `cache` serves a dual role:
  ;;   - While the consumer is waiting: holds the consumer's own
  ;;     continuation (captured by `(label)` inside
  ;;     `continue-with-pos?`), which is passed to `resume` so
  ;;     the search knows where to send the result.
  ;;   - After the search step completes: holds the packed result
  ;;     list (or `#f` if exhausted), which `pos->element` unpacks.
  (let ()
    (define ((make empty-sequence make-sequence) alt)
      (if (equal? alt amb*)
          empty-sequence
          (let ([first? #t])
            (define task* ((current-amb-maker)))
            (define length (current-amb-length))
            (define (empty-handler)
              (abort/cc amb-prompt-tag FALSE))
            (define (retry)
              (fail #:empty-handler empty-handler
                    #:tasks task*
                    #:length length))

            (define (next)
              ;; `next` produces the next search result as a list
              ;; (to accommodate multiple values uniformly).
              (cond
                ;; First call: run `alt` inside the parameterization
                ;; that was active when `in-amb*` was called, so that
                ;; `dynamic-wind`handlers, parameters, and other
                ;; context-sensitive features behave correctly
                ;; regardless of when the consumer forces the element.
                [first?
                 (set! first? #f)
                 (parameterize ([current-amb-prompt-tag amb-prompt-tag]
                                [current-amb-tasks task*]
                                [current-amb-empty-handler empty-handler])
                   (call-with-values alt list))]
                ;; Subsequent calls: trigger backtracking via `retry`
                ;; to the search to the next alternative.
                [else (call-with-values retry list)]))

            #;(: cache  (∪    (Option (Listof Any))     (¬ (Option (Listof Any))) ))
            #;(: resume (∪ (¬ (Option (Listof Any))) (¬ (¬ (Option (Listof Any))))))
            (define cache #f)
            (define resume (label))  ; the search-context entry point.
            (when cache
              ;; `cache`  : `(¬ (Option (Listof Any)))`
              ;; `resume` : `(¬ (Option (Listof Any)))`
              ;; Re-entry via `(goto resume cache)`:
              ;; `cache` holds the consumer's continuation
              ;; (the return address).  Run the search step inside
              ;; a fresh prompt so that `empty-handler` can abort
              ;; cleanly when all choices are exhausted.  Deliver the
              ;; result (a list or `#f`) back to the consumer by
              ;; jumping to the continuation stored in `cache`.
              (goto resume (call/prompt next amb-prompt-tag)))
            ;; `cache`  : `False`
            ;; `resume` : `(¬ (¬ (Option (Listof Any))))`
            ;; First entry:
            ;; Build and return the sequence object.
            (define (pos->element . _)
              ;; `cache` : `(Listof Any)`
              (apply values cache))
            (define (continue-with-pos? . _)
              (set! cache (label))
              (when (continuation? cache)
                ;; `cache` : `(¬ (Option (Listof Any)))`
                ;; Jump into search, pass return address
                (goto resume cache))
              ;; `cache` : `(Option (Listof Any))`
              ;; Back from search, cache now holds result
              (and cache #t))
            (make-sequence continue-with-pos? pos->element))))
    (values
     (make
      empty-stream
      (λ (continue-with-pos? pos->element)
        (for/stream ([_ (in-naturals)])
          #:break (not (continue-with-pos?))
          (pos->element))))
     (make
      empty-sequence
      (λ (continue-with-pos? pos->element)
        (make-do-sequence
         (λ ()
           (initiate-sequence
            #:init-pos 0
            #:next-pos add1
            #:continue-with-pos? continue-with-pos?
            #:pos->element pos->element))))))))
;; The previous implementation of `in-amb*` / `in-amb*/do`, kept here
;; for reference.  It used two nested unlimited `call/cc` captures.
#;
(define-values (in-amb* in-amb*/do)
  (let ()
    (define (make break-value make-sequence empty-sequence)
      (define continue-value (not break-value))
      (λ (alt)
        (if (equal? alt amb*)
            empty-sequence
            (let ([break #f] [return #f])
              ;; Convert a thunk producing multiple values into
              ;; a sequence by repeatedly invoking it while
              ;; maintaining a task queue.
              (define (empty-handler) (break break-value))
              (define length (current-amb-length))
              (define task* ((current-amb-maker)))
              (parameterize ([current-amb-empty-handler empty-handler]
                             [current-amb-tasks task*])
                (define task (label))
                (cond
                  [(not return)
                   ((current-amb-pusher) task* task)
                   (make-sequence
                    (λ (k) (set! break k) continue-value)
                    (λ (k) (set! return k)
                      (fail #:empty-handler empty-handler
                            #:tasks task*
                            #:length length)))]
                  [else
                   ((current-amb-popper) task*)
                   (call-with-values alt (λ v* (apply return v*)))]))))))
    (values
     (make #t
       empty-stream
       (λ (p1 p2)
         (for/stream ([_ (in-naturals)])
           #:break
           (call/cc p1)
           (call/cc p2))))
     (make #f
       empty-sequence
       (λ (p1 p2)
         (define (continue-with-pos? _) (call/cc p1))
         (define (pos->element _) (call/cc p2))
         (make-do-sequence
          (λ ()
            (initiate-sequence
             #:init-pos 0
             #:next-pos add1
             #:continue-with-pos? continue-with-pos?
             #:pos->element pos->element))))))))

(define-syntaxes (in-amb in-amb/do)
  ;; Macros expanding to calls to `in-amb*` or `in-amb*/do` with the
  ;; expression wrapped in a thunk.  They provide convenient sequence
  ;; syntax for iterating over ambiguous computations.
  (let ()
    (define ((make derived-stx) stx)
      (syntax-parse stx
        [(_ expr)
         (quasisyntax/loc stx
           (#,derived-stx (λ () expr)))]))
    (values (make #'in-amb*)
            (make #'in-amb*/do))))
