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
;;     #f         — dead end (all alternatives exhausted);
;;                  discard and continue searching
;;     #t         — nothing produced; push parent back and continue
;;                  searching
;;     (list ...) — leaf (computation produced values);
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
         "contract/base.rkt"
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
         current-amb-shuffler)
(provide unsafe-fail fail make-amb-tree thunks unsafe-amb*)


;; ---- Aliases ----

(define-for-syntax (append* v**) (apply append v**))

;; Alias for `vector`; used to construct the mutable vector of
;; alternative thunks in the `amb` macro expansion for clarity.
(define thunks vector)


;; ---- Parameters ----

(define current-amb-depth-first? (make-parameter #t  ))
(define current-amb-fair?        (make-parameter #f  ))
(define current-amb-shuffler     (make-parameter void))


;; ---- Prompt Tags ----

;; `amb-prompt-tag` delimits the leaf-labeled tree.  Every `shift-at`,
;; `fail`, and `make-amb-root` communicates through this prompt.
(define amb-prompt-tag (make-continuation-prompt-tag 'amb))


;; ---- Tree Construction ----

;; An `amb-tree` bundles a suspended continuation with the search
;; strategy parameters that were in effect at capture time.
;; `prop:procedure` makes the struct callable — `(t)` invokes `(k)`,
;; returning the next result in the tree protocol.
(struct amb-tree (k depth-first? fair?)
  #:property prop:procedure (struct-field-index k))

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
  (shift-at amb-prompt-tag k (amb-tree k depth-first? fair?)))

;; `make-amb-root` constructs the root `amb-tree` of a leaf-labeled
;; tree directly.  The `first?` flag produces exactly one leaf on the
;; first invocation and returns #f (dead end) on the second, so the
;; root is naturally discarded from the frontier after its sole result.
(define (make-amb-root thk)
  (define first? #t)
  (define depth-first? (current-amb-depth-first?))
  (define fair? (current-amb-fair?))
  (define (k)
    (reset-at amb-prompt-tag
      (and first?
           (begin (set! first? #f)
                  (call-with-values thk list)))))
  (amb-tree k depth-first? fair?))


;; ---- Failure ----

;; `unsafe-fail` aborts with the `none?` sentinel, signalling that the
;; current branch has no (more) solutions.  Unlike `make-amb-tree`,
;; `unsafe-fail` does not capture a continuation or produce an
;; `amb-tree` struct — `none?` returns #f directly, which the search
;; driver recognizes as a dead end.
(define (unsafe-fail) (abort/cc amb-prompt-tag none?))

;; `fail` aborts with the `any?` sentinel, which returns #t.
;; The search driver does not treat #t as a leaf, branch, or dead end,
;; so the calling tree is simply discarded and the search continues.
;; `fail` is also used as a sentinel value in the thunk vector of
;; `unsafe-amb*` to mark consumed slots.
(define (fail) (abort/cc amb-prompt-tag any?))


;; ---- Ambiguous Operator ----

;; `make-amb` is the foundational primitive for building
;; nondeterministic operators.  It captures the current continuation
;; as an `amb-tree`, then on each resumption calls `(more?)` to check
;; for remaining alternatives and `(get)` to produce the next value.
;; When `(more?)` returns #f, it calls `unsafe-fail` to signal a
;; dead end.
(define (make-amb more? get)
  (make-amb-tree)
  (if (more?) (get) (unsafe-fail)))

;; `unsafe-amb*` walks a mutable vector of thunks via `make-amb`,
;; one alternative per slot.
;;
;; The `more?` / `get` closures maintain a position `pos` into the
;; vector `thk*`.  Each consumed slot is overwritten with `fail` to
;; release the thunk for GC and to mark it as consumed.  When `get`
;; encounters a consumed slot, it advances to the next one.  When
;; `pos` reaches `len`, `thk*` is replaced with an empty one to free
;; the whole allocation.
;;
;; If all entries of the vector are `fail`, `fail` is called directly
;; without creating a tree.
;;
;; Two compaction passes keep the vector dense and its memory
;; footprint low:
;;
;;   Initial compaction — before the shuffler runs, binary search
;;   locates the first `fail` (contiguous at the tail).  The index is
;;   the length of the live prefix.  If it is 0 everything is dead; if
;;   it is >3/4 of the original length compaction isn't worthwhile;
;;   otherwise the live prefix is copied into a fresh vector.
;;
;;   Re-compaction — once at least half the entries have been
;;   consumed, the first re-compaction scans right-to-left to
;;   consolidate scattered `fail`s from shuffling at the front, then
;;   sets `pos` to the first non-`fail` entry.  Subsequent
;;   re-compactions use `vector-copy` from `pos` to `len`, since
;;   `fail`s are guaranteed contiguous at the head.
(define (unsafe-amb* thk*)
  (define len (vector-length thk*))

  (when (or (zero? len) (equal? (vector-ref thk* 0) fail))
    (set! thk* empty-mutable-vector)
    #;
    ((current-amb-shuffler) thk*)
    (fail))

  (define compacted? #f)
  (if (equal? (vector-ref thk* (sub1 len)) fail)
      (when (> len #xFF)
        ;; Binary search for the first `fail` — the boundary between
        ;; the live prefix and the dead suffix.  `fail`s are contiguous
        ;; at the tail before shuffling.
        (let loop ([lo 0] [hi len])
          (if (= lo hi)
              (when (< (* 4 lo) (* 3 len))
                (set! len lo)
                (set! thk* (vector-copy thk* 0 len))
                (set! compacted? #t))
              (let ([mid (arithmetic-shift (+ lo hi) -1)])
                (if (equal? (vector-ref thk* mid) fail)
                    (loop lo mid)
                    (loop (add1 mid) hi))))))
      (set! compacted? #t))

  ((current-amb-shuffler) thk*)
  (define pos 0)
  (define (more?) (not (= pos len)))
  (define (get)
    (define thk (vector-ref thk* pos))
    (vector-set! thk* pos fail)
    (set! pos (add1 pos))
    (cond
      [(equal? thk fail)
       (unless (more?)
         (set! thk* empty-mutable-vector)
         (unsafe-fail))
       (get)]
      [else
       (if (more?)
           (when (and (> len #xFF) (> (* pos 2) len))
             (cond
               ;; `fail`s are contiguous at the head; just slice the
               ;; live suffix.
               [compacted?
                (set! thk* (vector-copy thk* pos len))
                (set! len (vector-length thk*))
                (set! pos 0)]
               ;; First re-compaction: scattered `fail`s from shuffling
               ;; must be weeded out by a right-to-left scan.
               [else
                (define new-len (- len pos))
                (define new-thk* (make-vector new-len fail))
                (let loop ([i (sub1 len)] [j (sub1 new-len)])
                  (cond
                    [(>= i pos)
                     (define thk (vector-ref thk* i))
                     (cond
                       [(equal? thk fail)
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
     (syntax/loc stx (fail))]
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
;;    use `for/stream` to lazily evaluate the body of each iteration
;;    into a stream, then use `sequence-generate` to obtain a
;;    `more?`/`get` pair and delegate to `make-amb`.

(define-syntaxes (for/amb for*/amb)
  (let ()
    (define-splicing-syntax-class break-clause
      [pattern (~seq (~or* #:break #:final) guard:expr)])
    (define ((make-for/amb for/vector for/stream) stx)
      (syntax-parse stx
        [(_ #:length n (~optional (~seq #:fill fill-expr))
            (clauses ...) break-expr:break-clause ... body ...+)
         #:with fill
         (if (attribute fill-expr)
             #'(λ () fill-expr)
             #'fail)
         #:do
         [(define break
            (if (attribute break-expr)
                (append* (map syntax->list
                              (syntax->list #'(break-expr ...))))
                '()))]
         (quasisyntax/loc stx
           (let ([m n])
             (if (zero? m)
                 (fail)
                 (unsafe-amb*
                  (#,for/vector #:length n #:fill fill
                   (clauses ...) #,@break (λ () body ...))))))]
        [(_ (clauses ...) break-expr:break-clause ... body ...+)
         #:do
         [(define break
            (if (attribute break-expr)
                (append* (map syntax->list
                              (syntax->list #'(break-expr ...))))
                '()))]
         (quasisyntax/loc stx
           (let-values
               ([(more? get)
                 (sequence-generate
                  (#,for/stream (clauses ...) #,@break body ...))])
            (make-amb more? get)))]))
    (values (make-for/amb #'for/vector  #'for/stream)
            (make-for/amb #'for*/vector #'for*/stream))))


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
    ;;   #f        → dead end; discard `tree0` and continue searching
    ;;   #t        → nothing produced; push `tree0` back and continue
    ;;               search
    ;;   list?     → leaf; push `tree0` back (per its `fair?` field),
    ;;               return the result and suspend searching
    ;;   amb-tree? → branch; push `tree0` back (per its `fair?` field),
    ;;               enqueue `tree1` (per `tree0`'s `depth-first?` field),
    ;;               and continue searching
    (define (search frontier)
      (if (ideque-empty? frontier)
          (values #f frontier)
          (let-values ([(tree0 frontier) (pop frontier)])
            (define tree1 (tree0))
            (if (not tree1)
                (search frontier)
                (let* ([push (if (amb-tree-fair? tree0)
                                 ideque-add-back
                                 ideque-add-front)]
                       [frontier (push frontier tree0)])
                  (if (amb-tree? tree1)
                      (let* ([push (if (amb-tree-depth-first? tree0)
                                       ideque-add-front
                                       ideque-add-back)]
                             [frontier (push frontier tree1)])
                        (search frontier))
                      (if (or (pair? tree1) (null? tree1))
                          (values tree1 frontier)
                          (search frontier))))))))
    (define ((make make-sequence empty-sequence raise-empty-sequence-error) thk)
      (if (equal? thk fail)
          empty-sequence
          (make-sequence
           (λ ()
             (let loop ([frontier (ideque (make-amb-root thk))])
               (let-values ([(v* frontier) (search frontier)])
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
           (#,derived-stx (λ () expr)))]))
    (values (make #'in-amb*)
            (make #'in-amb*/do))))
