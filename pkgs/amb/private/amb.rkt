#lang racket/base

;; Core implementation of the ambiguous operator.  This module
;; defines the runtime primitives used by the public API in
;; "amb/main.rkt" and its typed counterpart.

(require "utils.rkt"
         (for-syntax racket/base syntax/parse)
         racket/case
         racket/sequence
         racket/stream
         goto)

(provide empty-mutable-vector
         amb amb* unsafe-amb*
         for/amb for*/amb
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
  ;; Process a vector of thunks sequentially, using the task queue to
  ;; backtrack whenever a thunk signals failure via `amb`.
  (define len (vector-length alt*))
  (define length (current-amb-length))
  (define task* (current-amb-tasks))
  ((current-amb-shuffler) alt*)    ; allow user provided randomization
  (when (zero? len)
    (fail #:tasks task*
          #:length length))
  (define pos #t)
  (define task (label (current-amb-prompt-tag)))
  (case/eq pos
    ;; first entry
    [(#t)
     (set! pos 0)
     ((current-amb-pusher) task* task)
     (goto (sequence-ref task* 0))]
    ;; no more alternatives
    [(#f)
     (fail #:tasks task*
           #:length length)]
    [else
     (when (= pos len)
       (set! pos #f)
       ((current-amb-popper) task*)
       (fail #:tasks task*
             #:length length))])
  (define alt (vector-ref alt* pos))
  (vector-set! alt* pos amb*)
  (set! pos (add1 pos))
  (when (= pos len)
    (set! alt* empty-mutable-vector))
  (when (equal? alt amb*)
    (goto task))
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
      (syntax-parse stx
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
                   (λ () body ...))))))]
        [(_ (clauses ...) break:break-clause ... body ...+)
         (quasisyntax/loc stx
           (call/cc
            (λ (return)
              (define retry #t)
              (define length (current-amb-length))
              (define task* (current-amb-tasks))
              (define task (label (current-amb-prompt-tag)))
              (cond
                [(continuation? retry)
                 (goto retry)]
                ;; first entry
                [retry
                 (set! retry #f)
                 ((current-amb-pusher) task* task)
                 (goto (sequence-ref task* 0))])
              (#,for (clauses ...) break ...
               (define choice (label (current-amb-prompt-tag)))
               (unless (and retry (eq? retry choice))
                 (set! retry choice)
                 ((current-amb-rotator) task*)
                 (call-in-continuation return (λ () body ...))))
              ;; no more alternatives
              ((current-amb-popper) task*)
              (define skip (label (current-amb-prompt-tag)))
              (unless (eq? retry skip)
                (set! retry skip))
              (fail #:tasks task*
                    #:length length))
            (current-amb-prompt-tag)))]))
    (values (make-for/amb #'for/vector  #'for)
            (make-for/amb #'for*/vector #'for*))))


(define-values (in-amb* in-amb*/do)
  (let ()
    (define ((make make-sequence) alt)
      ;; Cache the most recently produced values so the sequence
      ;; interface can retrieve them after the search step.
      (define cache #f)
      (define task* ((current-amb-maker)))
      (define length (current-amb-length))
      (define (empty-handler) (abort/cc amb-prompt-tag FALSE))
      (define (retry)
        (fail #:empty-handler empty-handler
              #:tasks task*
              #:length length))

      (define (init)
        ;; Run the user's thunk inside the parameterization that was
        ;; in effect when `in-amb*` was called.  This ensures that
        ;; parameters such as `current-amb-tasks` and all user-defined
        ;; parameters are correctly scoped to this sequence,
        ;; regardless of the parameterization in effect when the
        ;; sequence is later consumed.
        (parameterize ([current-amb-prompt-tag amb-prompt-tag]
                       [current-amb-tasks task*]
                       [current-amb-empty-handler empty-handler])
          (call-with-values alt list)))

      ;; `freeze` is a full continuation capturing the context of this
      ;; `in-amb*` call.  It serves as the entry point into the
      ;; search: jumping to `freeze` re-enters the function body and
      ;; takes the `else` branch below, which runs the actual search.
      ;;
      ;; `resume` is set later (inside `next`) to capture the context
      ;; of the sequence consumer waiting for a result.  It serves as
      ;; the exit point out of the search: once a result is produced,
      ;; control jumps back to `resume` to deliver the value.
      (define resume #f)
      (define freeze (label))
      (define (next)
        ;; Called by the sequence machinery on each step.
        (if cache
            ;; Subsequent calls:
            ;; cache holds the previous result, so trigger backtracking
            ;; via `retry` to produce the next one.
            (call-with-values retry list)
            ;; First call:
            ;; no cache yet, so capture the consumer's continuation as
            ;; `resume` and jump to `freeze` to begin search.
            (let/cc k (set! resume k) (goto freeze))))

      (cond
        [(not resume)
         ;; First entry: set up the sequence interace and register
         ;; `freeze` in the queue so that backtracking from within the
         ;; search can jump back here when needed.
         (define (update! v*) (set! cache v*) #t)
         (define (pos->element . _) (apply values cache))
         (define (continue-with-pos? . _)
           (cond
             [(call/prompt next amb-prompt-tag) => update!]
             [else (set! cache #f) #f]))
         (make-sequence continue-with-pos? pos->element)]
        [else
         ;; Subsequent entries via `(goto freeze)`: the search is about
         ;; to run for real.  Run `init` inside a fresh prompt so that
         ;; any exhaustion aborts cleanly.  When `init` returns values,
         ;; deliver them to `resume`, jumping back to the consumer.
         (call-with-values
          (λ ()
            (set! resume #f)
            (call/prompt init amb-prompt-tag))
          resume)]))
    (values
     (make
      (λ (continue-with-pos? pos->element)
        (for/stream ([_ (in-naturals)])
          #:break (not (continue-with-pos?))
          (pos->element))))
     (make
      (λ (continue-with-pos? pos->element)
        (make-do-sequence
         (λ ()
           (initiate-sequence
            #:init-pos 0
            #:next-pos add1
            #:continue-with-pos? continue-with-pos?
            #:pos->element pos->element))))))))
#;
(define-values (in-amb* in-amb*/do)
  (let ()
    (define (make break-value make-sequence)
      (define continue-value (not break-value))
      (λ (alt)
        ;; Convert a thunk producing multiple values into a sequence
        ;; by repeatedly invoking it while maintaining a task queue.
        (define break #f)
        (define return #f)
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
             (call-with-values alt (λ v* (apply return v*)))]))))
    (values
     (make #t
       (λ (p1 p2)
         (for/stream ([_ (in-naturals)])
           #:break
           (call/cc p1)
           (call/cc p2))))
     (make #f
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
