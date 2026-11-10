#lang racket/base

;; ===================================================================
;; Nondeterministic Choice via Delimited Continuations
;; ===================================================================
;;
;; This module implements the ambiguous operator `amb` and its
;; companions using composable continuations and an immutable
;; double-ended queue (deque) as the search frontier.
;;
;; All `amb` operations must occur within the dynamic extent of
;; `in-amb` (or `in-amb/do`), which installs the continuation prompt
;; that delimits the leaf-labeled tree.
;;
;; --- Tree Protocol ---
;;
;; A *tree* is a struct bundling a suspended continuation `k` with the
;; search strategy that was in effect when the tree was created:
;;
;;   (struct amb-tree (k depth-first? fair?))
;;
;; Thanks to `prop:procedure`, an `amb-tree` `t` is directly callable:
;; `(t)` invokes `(k)` and returns one of:
;;
;;     dead       — dead end (all alternatives exhausted); a private
;;                  value only `unsafe-fail` produces; discard and
;;                  continue searching
;;     #f         — nothing produced (`amb-fail`, which is also what
;;                  `(amb)` does under the default empty handler);
;;                  push parent back and continue searching
;;     (list ...) — leaf (the computation produced values, or `(amb)`
;;                  was answered by the search's empty handler);
;;                  push parent back and suspend searching
;;     amb-tree   — branch (child tree to be enqueued);
;;                  push parent back, enqueue child, and continue
;;                  searching
;;
;; The `depth-first?` and `fair?` fields record the values of
;; `current-amb-depth-first?` and `current-amb-fair?` at the point
;; where `make-amb-tree` was called.  The search driver reads these
;; fields to decide where to insert child and parent trees in the
;; frontier deque.  This allows different sub-computations within the
;; same leaf-labeled tree to use different strategies via
;; `parameterize`.
;;
;; --- Search Strategy ---
;;
;; Two orthogonal boolean parameters control how the frontier deque
;; is managed.  Their values are captured per-tree at creation time:
;;
;;   current-amb-depth-first?
;;     #t (default) — push child trees to the front  (DFS)
;;     #f           — push child trees to the back   (BFS)
;;
;;   current-amb-fair?
;;     #f (default) — put the parent tree back at the front
;;     #t           — put the parent tree back at the back
;;                    (prevents starvation of sibling branches)

(require (for-syntax racket/base syntax/parse)
         racket/control
         "data/mutable-vector.rkt"
         "data/sequence.rkt"
         "data/stream.rkt"
         "data/ideque.rkt")

(provide make-amb
         amb for/amb for*/amb
         in-amb  in-amb/do
         in-amb* in-amb*/do
         current-amb-depth-first?
         current-amb-fair?
         current-amb-local?
         current-amb-shuffler)
(provide amb-fail amb-empty-handler unsafe-fail make-amb-tree thunks unsafe-amb*)


;; ---- Aliases ----

(define-for-syntax (append* v**) (apply append v**))

;; Alias for `vector`; used to construct the mutable vector of
;; alternative thunks in the `amb` macro expansion for clarity.
(define thunks vector)


;; ---- Parameters ----

(define current-amb-depth-first? (make-parameter #t  ))
(define current-amb-fair?        (make-parameter #f  ))
;; RESEARCH (local frontiers): a choice point created while this is #t
;; schedules its descendants itself, in a frontier of its own.
(define current-amb-local?       (make-parameter #f  ))
(define current-amb-shuffler     (make-parameter void))


;; ---- Prompt Tags ----

;; `amb-prompt-tag` delimits the leaf-labeled tree.  Every `shift-at`,
;; `amb-fail`, `unsafe-fail`, `amb-empty-handler`, and `make-amb-root`
;; communicates through this prompt; `amb-empty-handler` also looks up
;; the search's empty handler no further out than it.
(define amb-prompt-tag (make-continuation-prompt-tag 'amb))


;; ---- Tree Construction ----

;; An `amb-tree` bundles a suspended continuation with the search
;; strategy parameters that were in effect at capture time.
;; `prop:procedure` makes the struct callable — `(t)` invokes `(k)`,
;; returning the next result in the tree protocol.
(struct amb-tree (k depth-first? fair? local?)
  #:property prop:procedure (struct-field-index k))

;; RESEARCH (local frontiers).  A tree created with `current-amb-local?`
;; is kept in the frontier as an `amb-node`: the tree together with a
;; frontier of its own, which holds the tree itself and every tree
;; created below it.  To the frontier that contains it, a node is one
;; item: each time it is resumed it runs its own frontier until that
;; produces a leaf or yields.  So the frontiers form a tree of the same
;; shape as the choice points that asked for one.
(struct amb-node (tree [frontier #:mutable]))

;; `make-amb-tree` snapshots the current strategy parameters and uses
;; `shift-at` to capture the continuation, packaging everything into
;; an `amb-tree` struct.  The struct is returned to the enclosing
;; `reset-at` (in `make-amb-root`, or in a resumed tree's own `k`).
;;
;; When the search driver later calls `(t)` on this tree, the captured
;; continuation resumes from the point after `shift-at`, under a fresh
;; prompt.
(define (make-amb-tree)
  (define depth-first? (current-amb-depth-first?))
  (define fair? (current-amb-fair?))
  (define local? (current-amb-local?))
  (shift-at amb-prompt-tag k (amb-tree k depth-first? fair? local?)))

;; `make-amb-root` constructs the root `amb-tree` of a leaf-labeled
;; tree directly.  The `first?` flag produces exactly one leaf on the
;; first invocation and returns `dead` on the second, so the
;; root is naturally discarded from the frontier after its sole result.
;;
;; The search's empty handler (see `amb-empty-handler`) is attached to
;; the root frame as a continuation mark.  Every continuation of this
;; search is captured inside that frame, so the mark comes back whenever
;; a tree of this search is resumed, and an inner search's own mark is
;; found first.  The default handler, `amb-fail`, is not attached: the
;; lookup stops at this search's prompt and falls back to it anyway.
(define (make-amb-root thk [handler amb-fail])
  (define first? #t)
  (define depth-first? (current-amb-depth-first?))
  (define fair? (current-amb-fair?))
  (define (k)
    (reset-at amb-prompt-tag
      (cond
        [first?
         (set! first? #f)
         (if (eq? handler amb-fail)
             (call-with-values thk list)
             ;; Not in tail position of `reset-at`: a mark on the
             ;; prompt's own frame can reappear innermost when a
             ;; continuation captured across another prompt is applied
             ;; again (seen on Racket v9.3 CS), which would hide the
             ;; `reentrant-amb` mark of a running handler.
             (let ([vs (with-continuation-mark empty-handler-key handler
                         (call-with-values thk list))])
               vs))]
        [else dead])))
  (amb-tree k depth-first? fair? #f))


;; ---- Failure ----

;; `unsafe-fail` aborts with the private `dead` value, signalling that the
;; current branch has no (more) solutions.  Unlike `make-amb-tree`,
;; `unsafe-fail` does not capture a continuation or produce an
;; `amb-tree` struct; the search driver recognizes `dead` as a dead
;; end and discards the tree being resumed.
;; `dead` is private: no user code can produce it, so only the
;; library's own generators can drop a tree from the frontier.
(define dead (string->uninterned-symbol "dead"))
(define (dead-thunk) dead)
(define (unsafe-fail) (abort/cc amb-prompt-tag dead-thunk))

;; `(amb-fail)` is failure: it aborts with #f, which the search
;; driver treats as "nothing produced": it pushes the calling tree
;; back onto the frontier (respecting its `fair?` field) and continues
;; searching.  This is precisely what lets a single failed alternative
;; fail without discarding its parent — e.g. `(amb (amb-fail) 5)` still
;; yields 5, because the inner `(amb-fail)` fails but the parent tree
;; survives to produce 5.
;; `amb-fail` is also the sentinel marking consumed slots in the thunk
;; vector of `unsafe-amb*`: as a thunk it fails.
(define (amb-fail) (abort/cc amb-prompt-tag false-thunk))
(define (false-thunk) #f)

;; `(amb)` is a choice among no alternatives: the computation ends
;; here.  What it ends *with* is decided by the innermost search's
;; *empty handler*, given as `#:empty-handler` to `in-amb` / `in-amb/do`
;; (default `amb-fail`).  `amb-empty-handler` looks that handler up,
;; calls it at the point of `(amb)`, in its dynamic context, and outputs
;; the values it returns as one answer of the search; with the default
;; handler, `(amb)` is `(amb-fail)`.  The handler belongs to the search,
;; not to the dynamic extent, so an inner search that was given no
;; handler fails at its own `(amb)` even inside an outer one that was.
;; Either way `amb-empty-handler` never returns, and the tree being
;; resumed is pushed back (per its `fair?`).
;; For example, (when (amb #t #f) (amb)) is a pause: the search outputs
;; the handler's answer, then continues after it.
;;
;; While the handler runs, the mark is replaced with `reentrant-amb`, so
;; a handler that itself reaches `(amb)` of the same search raises an
;; error instead of calling itself forever.  An inner search inside the
;; handler has its own mark and is not affected.
(define empty-handler-key (make-continuation-mark-key 'amb-empty-handler))
(define (amb-empty-handler)
  (define handler
    (continuation-mark-set-first #f empty-handler-key amb-fail amb-prompt-tag))
  (cond
    [(eq? handler amb-fail) (abort/cc amb-prompt-tag yield-thunk)]
    [else
     (define vs
       (call-with-values
        (λ () (with-continuation-mark empty-handler-key reentrant-amb (handler)))
        list))
     (abort/cc amb-prompt-tag (λ () (yielded vs)))]))
;; RESEARCH (local frontiers).  `(amb)` is reported to the driver as a
;; *yield*, with the handler's values if it has any, so that a node can
;; tell it from a failure or an ordinary leaf: the node that yields
;; gives up its turn (it goes to the back of the frontier that contains
;; it), and so does every node around it.  Outside any node a yield is
;; what it was before: a failure, or a leaf.
(struct yielded (vs))
(define yield-nothing (string->uninterned-symbol "yield"))
(define (yield-thunk) yield-nothing)
;; A misuse of the handler protocol, so `exn:fail:contract`, like the
;; error for `(amb)` outside any search.
(define (reentrant-amb)
  (raise-arguments-error
   'amb "(amb) reached inside the empty handler of the same search"))


;; ---- Ambiguous Operator ----

;; `make-amb` is the foundational primitive for building
;; nondeterministic operators.  It captures the current continuation
;; as an `amb-tree`, then on each resumption calls `(more?)` to check
;; for remaining alternatives and `(get)` to produce the next value.
;; When `(more?)` returns #f, it calls `unsafe-fail` to signal a
;; dead end.
;;
;; A choice that has no alternative at all is `(amb)`; once it has
;; produced one, running out is a dead end.  `state` is #f until the
;; first `(more?)`, then 'started or 'empty.  After `(amb)` has been
;; answered the tree is still resumed once more (it was pushed back);
;; that resumption must neither call `(more?)` again nor answer `(amb)`
;; twice, so 'empty is checked first and is a dead end.
(define (make-amb more? get)
  (define state #f)
  (make-amb-tree)
  (cond
    [(eq? state 'empty) (unsafe-fail)]
    [(more?) (set! state 'started) (get)]
    [state (unsafe-fail)]
    [else (set! state 'empty) (amb-empty-handler)]))

;; `unsafe-amb*` walks a mutable vector of thunks via `make-amb`,
;; one alternative per slot.
;;
;; The `more?` / `get` closures maintain a position `pos` into the
;; vector `thk*`.  Each consumed slot is overwritten with `amb-fail` to
;; release the thunk for GC and to mark it as consumed.  When `get`
;; encounters a consumed slot, it advances to the next one.  When
;; `pos` reaches `len`, `thk*` is replaced with an empty one to free
;; the whole allocation.
;;
;; PRECONDITION: on entry, `amb-fail` sentinels are contiguous at the tail
;; of `thk*` (equivalently, the live thunks form a prefix).  Both call
;; sites uphold this — the `amb` macro builds an all-live vector, and
;; `for/amb #:length` fills only the unused *trailing* slots with
;; `amb-fail` via `#:fill`.  This precondition is why testing slot 0 alone
;; suffices to detect a wholly-dead vector: if `thk*[0]` is `amb-fail`,
;; then by tail-contiguity every slot is `amb-fail`.  (Note this holds
;; only at entry; once the shuffler runs, `amb-fail`s may be scattered,
;; which is what the re-compaction scan below handles.)
;;
;; If the vector is empty or wholly dead, the choice has no alternative
;; at all: it is `(amb)`, so `amb-empty-handler` is called directly
;; (which outputs the handler's answer, or fails under the default
;; handler, and pushes the parent tree back) without creating a tree.
;; The shuffler is not called: there is nothing to shuffle.
;;
;; Two compaction passes keep the vector dense and its memory
;; footprint low:
;;
;;   Initial compaction — before the shuffler runs, binary search
;;   locates the first `amb-fail` (contiguous at the tail).  The index is
;;   the length of the live prefix.  If it is 0 everything is dead; if
;;   it is >3/4 of the original length compaction isn't worthwhile;
;;   otherwise the live prefix is copied into a fresh vector.
;;
;;   Re-compaction — once at least half the entries have been
;;   consumed, the first re-compaction scans right-to-left to
;;   consolidate scattered `amb-fail`s from shuffling at the front, then
;;   sets `pos` to the first non-`amb-fail` entry.  Subsequent
;;   re-compactions use `vector-copy` from `pos` to `len`, since
;;   `amb-fail`s are guaranteed contiguous at the head.
(define (unsafe-amb* thk*)
  (define len (vector-length thk*))

  (when (or (zero? len) (equal? (vector-ref thk* 0) amb-fail))
    (set! thk* empty-mutable-vector)
    (amb-empty-handler))

  (define compacted? #f)
  (if (equal? (vector-ref thk* (sub1 len)) amb-fail)
      (when (> len #xFF)
        ;; Binary search for the first `amb-fail` — the boundary between
        ;; the live prefix and the dead suffix.  `amb-fail`s are contiguous
        ;; at the tail before shuffling.
        (let loop ([lo 0] [hi len])
          (if (= lo hi)
              (when (< (* 4 lo) (* 3 len))
                (set! len lo)
                (set! thk* (vector-copy thk* 0 len))
                (set! compacted? #t))
              (let ([mid (arithmetic-shift (+ lo hi) -1)])
                (if (equal? (vector-ref thk* mid) amb-fail)
                    (loop lo mid)
                    (loop (add1 mid) hi))))))
      (set! compacted? #t))

  ((current-amb-shuffler) thk*)
  (define pos 0)
  (define (more?) (not (= pos len)))
  (define (get)
    (define thk (vector-ref thk* pos))
    (vector-set! thk* pos amb-fail)
    (set! pos (add1 pos))
    (cond
      [(equal? thk amb-fail)
       (unless (more?)
         (set! thk* empty-mutable-vector)
         (unsafe-fail))
       (get)]
      [else
       (if (more?)
           (when (and (> len #xFF) (> (* pos 2) len))
             (cond
               ;; `amb-fail`s are contiguous at the head; just slice the
               ;; live suffix.
               [compacted?
                (set! thk* (vector-copy thk* pos len))
                (set! len (vector-length thk*))
                (set! pos 0)]
               ;; First re-compaction: scattered `amb-fail`s from shuffling
               ;; must be weeded out by a right-to-left scan.
               [else
                (define new-len (- len pos))
                (define new-thk* (make-vector new-len amb-fail))
                (let loop ([i (sub1 len)] [j (sub1 new-len)])
                  (cond
                    [(>= i pos)
                     (define thk (vector-ref thk* i))
                     (cond
                       [(equal? thk amb-fail)
                        (loop (sub1 i) j)]
                       [else
                        (vector-set! new-thk* j thk)
                        (loop (sub1 i) (sub1 j))])]
                    [else
                     (set! thk* new-thk*)
                     (set! len new-len)
                     (set! pos (add1 j))
                     (set! compacted? #t)]))]))
           (begin
             (set! thk* empty-mutable-vector)
             (set! len 0)
             (set! pos 0)))
       (thk)]))
  (make-amb more? get))

;; Syntax form: each expression is implicitly wrapped in a thunk,
;; collected into a mutable vector, then passed to `unsafe-amb*`.
(define-syntax (amb stx)
  (syntax-parse stx
    [(_)
     (syntax/loc stx (amb-empty-handler))]
    [(_ expr* ...)
     (syntax/loc stx (unsafe-amb* (thunks (λ () expr*) ...)))]))


;; ---- for/amb: Nondeterministic Choice over Sequences ----
;;
;; Two compilation strategies:
;;
;; 1. With `#:length` — the number of iterations is known upfront,
;;    so we collect all body thunks into a vector via `for/vector`,
;;    then delegate to `unsafe-amb*`.
;;
;; 2. Without `#:length` — the iteration count is unknown, so we
;;    run the `for` loop as a coroutine: a per-site prompt tag lets
;;    each iteration `abort` out with a single body thunk, and a
;;    saved escape continuation (`retry`) re-enters the loop to
;;    resume iteration on the next resumption.  This avoids the
;;    per-element stream-cons allocation that a `for/stream` +
;;    `sequence-generate` pipeline would incur, and matches the
;;    mechanism used by the typed front-end (which lacks `for/stream`).
;;
;;    Because the body is deferred to the `alt` thunk and only
;;    invoked *after* the abort has left the loop's dynamic extent,
;;    a nested `amb` in the body captures a clean continuation and
;;    does not corrupt the loop state.

(define-syntaxes (for/amb for*/amb)
  (let ()
    (define-splicing-syntax-class break-clause
      [pattern (~seq (~or* #:break #:final) guard:expr)])
    (define ((make-for/amb for/vector for) stx)
      (syntax-parse stx
        [(_ #:length n (~optional (~seq #:fill fill-expr))
            (clauses ...) break-expr:break-clause ... body ...+)
         #:with fill
         (if (attribute fill-expr)
             #'(λ () fill-expr)
             #'amb-fail)
         #:do
         [(define break
            (if (attribute break-expr)
                (append* (map syntax->list
                              (syntax->list #'(break-expr ...))))
                '()))]
         (quasisyntax/loc stx
           (unsafe-amb*
            (let ([m n])
              (if (zero? m)
                  empty-mutable-vector
                  (#,for/vector #:length m #:fill fill
                   (clauses ...) #,@break (λ () body ...))))))]
        [(_ (clauses ...) break-expr:break-clause ... body ...+)
         #:do
         [(define break
            (if (attribute break-expr)
                (append* (map syntax->list
                              (syntax->list #'(break-expr ...))))
                '()))]
         (quasisyntax/loc stx
           (let ([prompt-tag (make-continuation-prompt-tag)])
             (call-with-continuation-prompt
              (λ ()
                (define retry #f)
                (define started? #f)
                (make-amb-tree)
                ;; Resumed after `(amb)` was answered (no alternative was
                ;; produced, so `retry` is #f): a dead end.  Re-entering the
                ;; loop would evaluate its clauses again.
                (cond
                  [retry (retry #f)]
                  [started? (unsafe-fail)])
                (#,for (clauses ...) #,@break
                 (define choice (call/cc values prompt-tag))
                 (when choice
                   (define (alt) body ...)
                   (set! retry choice)
                   (set! started? #t)
                   (abort/cc prompt-tag alt)))
                (set! retry #f)
                ;; no alternative at all: this for/amb is (amb)
                (cond
                  [started? (unsafe-fail)]
                  [else (set! started? #t) (amb-empty-handler)]))
              prompt-tag)))]))
    (values (make-for/amb #'for/vector  #'for)
            (make-for/amb #'for*/vector #'for*))))


;; ---- Search Driver ----
;;
;; `in-amb*` and `in-amb*/do` convert a nondeterministic computation
;; into a lazy stream or a do-sequence respectively.

(define-values (in-amb* in-amb*/do)
  (let ([pop ideque-pop-front])
    ;; The `search` function walks the frontier deque until a leaf is
    ;; found or the deque is empty.  For each dequeued tree `tree0`,
    ;; it calls `(tree0)` and inspects the result `tree1`:
    ;;
    ;;   dead      → dead end; discard `tree0` and continue searching
    ;;   #f        → nothing produced; push `tree0` back and continue
    ;;               search
    ;;   list?     → leaf; push `tree0` back (per its `fair?` field),
    ;;               return the result and suspend searching
    ;;   amb-tree? → branch; push `tree0` back (per its `fair?` field),
    ;;               enqueue `tree1` (per `tree0`'s `depth-first?` field),
    ;;               and continue searching
    ;;
    ;; RESEARCH (local frontiers).  An item of a frontier is a tree or
    ;; an `amb-node`.  A node is resumed by running its own frontier
    ;; with this same function (`root?` #f), which stops at a leaf or a
    ;; yield and passes it on.  A child tree that asked for a frontier
    ;; of its own is wrapped in a node when it is enqueued.  On a yield,
    ;; the item goes to the back whatever its `fair?` field says, if it
    ;; is a node or if it is inside a node's frontier; a plain tree in
    ;; the root frontier follows its `fair?` field as before.  At the
    ;; root a yield is unwrapped: its values are a leaf, and without
    ;; values the search goes on.
    (define (search frontier root?)
      (if (ideque-empty? frontier)
          (values #f frontier)
          (let*-values ([(item0 frontier) (pop frontier)]
                        [(node?) (amb-node? item0)]
                        [(tree1) (if node? (resume-node item0) (item0))]
                        ;; a node left with a single node inside it is
                        ;; that node
                        [(item) (if node? (collapse item0) item0)]
                        [(tree0) (if node? (amb-node-tree item) item)])
            (cond
              [(eq? tree1 dead) (search frontier root?)]
              [(amb-tree? tree1)
               (let* ([push (if (amb-tree-fair? tree0)
                                ideque-add-back
                                ideque-add-front)]
                      [frontier (push frontier item)]
                      [push (if (amb-tree-depth-first? tree0)
                                ideque-add-front
                                ideque-add-back)]
                      [child (if (amb-tree-local? tree1)
                                 (amb-node tree1 (ideque tree1))
                                 tree1)]
                      [frontier (push frontier child)])
                 (search frontier root?))]
              [(or (eq? tree1 yield-nothing) (yielded? tree1))
               (let* ([push (if (or node? (not root?) (amb-tree-fair? tree0))
                                ideque-add-back
                                ideque-add-front)]
                      [frontier (push frontier item)])
                 (cond
                   [(not root?) (values tree1 frontier)]
                   [(yielded? tree1) (values (yielded-vs tree1) frontier)]
                   [else (search frontier root?)]))]
              [else
               (let* ([push (if (amb-tree-fair? tree0)
                                ideque-add-back
                                ideque-add-front)]
                      [frontier (push frontier item)])
                 (cond
                   [(or (pair? tree1) (null? tree1))
                    (values tree1 frontier)]
                   [(not tree1) (search frontier root?)]
                   [else (error 'amb "internal: unexpected tree result ~e" tree1)]))]))))
    ;; One turn of a node: run its frontier up to a leaf or a yield.  A
    ;; node whose frontier is exhausted is a dead end.
    (define (collapse node)
      (define frontier (amb-node-frontier node))
      (if (= (ideque-length frontier) 1)
          (let ([inner (ideque-front frontier)])
            (if (amb-node? inner) (collapse inner) node))
          node))
    (define (resume-node node)
      (define-values (result frontier) (search (amb-node-frontier node) #f))
      (set-amb-node-frontier! node frontier)
      (or result dead))
    (define ((make make-sequence empty-sequence raise-empty-sequence-error)
             thk [handler amb-fail])
      (if (equal? thk amb-fail)
          empty-sequence
          (make-sequence
           (λ ()
             (let loop ([frontier (ideque (make-amb-root thk handler))])
               (let-values ([(v* frontier) (search frontier #t)])
                 (values v* (if v* (λ () (loop frontier)) raise-empty-sequence-error))))))))
    (values (make make-stream   empty-stream   raise-empty-stream-error)
            (make make-sequence empty-sequence raise-empty-sequence-error))))

;; Syntax forms: wrap the expression in a thunk.
(define-syntaxes (in-amb in-amb/do)
  (let ()
    (define ((make derived-stx) stx)
      (syntax-parse stx
        [(_ expr)
         (quasisyntax/loc stx
           (#,derived-stx (λ () expr)))]
        [(_ expr #:empty-handler handler)
         (quasisyntax/loc stx
           (#,derived-stx (λ () expr) handler))]))
    (values (make #'in-amb*)
            (make #'in-amb*/do))))
