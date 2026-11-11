#lang racket/base

;; ===================================================================
;; Nondeterministic Choice via Delimited Continuations
;; ===================================================================
;;
;; This module implements the ambiguous operator `amb` and its
;; companions using composable continuations and frontiers: mutable
;; handles on immutable double-ended queues (deques) of pending trees.
;;
;; All `amb` operations must occur within the dynamic extent of
;; `in-amb` (or `in-amb/do`), which installs the continuation prompt
;; that delimits the leaf-labeled tree.
;;
;; --- Tree Protocol ---
;;
;; A *tree* is a struct bundling a suspended continuation `k` with the
;; search strategy and the frontier that were in effect when the tree
;; was created:
;;
;;   (struct amb-tree (k depth-first? fair? frontier))
;;
;; Thanks to `prop:procedure`, an `amb-tree` `t` is directly callable:
;; `(t)` invokes `(k)` and returns one of:
;;
;;     dead       — dead end (all alternatives exhausted); a private
;;                  value only `unsafe-fail` produces; discard and
;;                  continue searching
;;     #f         — nothing produced (`amb-fail`);
;;                  push parent back and continue searching
;;     (list ...) — leaf (the computation produced values);
;;                  push parent back and suspend searching
;;     yielded    — yield (`(amb)`), carrying the values of the empty
;;                  handler of the frontier `(amb)` was called in, or
;;                  #f under the default handler; push parent back,
;;                  then treat as a leaf or as nothing produced.  A
;;                  yield differs from those two only inside a local
;;                  frontier (see below)
;;     amb-tree   — branch (child tree to be enqueued);
;;                  push parent back, enqueue child, and continue
;;                  searching
;;
;; The `depth-first?` and `fair?` fields record the values of
;; `current-amb-depth-first?` and `current-amb-fair?` at the point
;; where `make-amb-tree` was called.  The search driver reads these
;; fields to decide where to insert child and parent trees in the
;; frontier.  This allows different sub-computations within the same
;; leaf-labeled tree to use different strategies via `parameterize`.
;;
;; --- Search Strategy ---
;;
;; Two orthogonal boolean parameters control how a frontier is
;; managed.  Their values are captured per-tree at creation time:
;;
;;   current-amb-depth-first?
;;     #t (default) — push child trees to the front  (DFS)
;;     #f           — push child trees to the back   (BFS)
;;
;;   current-amb-fair?
;;     #f (default) — put the parent tree back at the front
;;     #t           — put the parent tree back at the back
;;                    (prevents starvation of sibling branches)
;;
;; --- Frontiers ---
;;
;; A *frontier* (`amb-frontier`) is a first-class, mutable handle on a
;; deque of items, together with an *empty handler*, which answers
;; `(amb)`.  An item is a tree or a *mount* of another frontier: so the
;; frontiers form a tree, and each tree records the frontier it was
;; born in (the innermost one marked where `make-amb-tree` is called).
;;
;;   frontier ──▶ item*        (an item is a tree or a mounted frontier)
;;   tree     ──▶ frontier     (where it was born)
;;
;; `in-amb` runs a *root* frontier, made from its `#:empty-handler` or
;; given by `#:frontier`.  `amb/local` makes a fresh frontier `f` and
;; has the trees created in its extent born in `f`.  When a child tree
;; is born in a frontier that the driver is not running at the moment,
;; the child is pushed into that frontier, and the frontier is
;; *mounted*, as one item, in the frontier being run.  A child born in a frontier that is being
;; run (the current one, or one further out, such as the code after an
;; `amb/local` has returned) is pushed into the current one.  Mounting
;; again after a frontier ran out is allowed, and so is having one
;; frontier mounted in several others; turns follow the driver's call
;; stack, never parent pointers.
;;
;; A mounted frontier takes a turn by running its own items up to the
;; next leaf or yield.  A yield ends the turn of every mounted frontier
;; around it: each goes to the back of the frontier that contains it.
;; So `(amb)` is where the branches of a local choice take turns, level
;; by level, and the resulting order depends only on the tree of
;; choices.

(require (for-syntax racket/base syntax/parse)
         racket/control
         "data/mutable-vector.rkt"
         "data/sequence.rkt"
         "data/stream.rkt"
         "data/ideque.rkt")

;; ---- Exports ----

(provide make-amb
         amb for/amb for*/amb
         amb/local
         in-amb  in-amb/do
         in-amb* in-amb*/do
         current-amb-depth-first?
         current-amb-fair?
         current-amb-shuffler
         make-amb-frontier amb-frontier?
         amb-frontier-length amb-frontier-empty-handler)
(provide amb-fail amb-empty-handler unsafe-fail make-amb-tree thunks unsafe-amb*
         call-with-local-frontier)


;; ---- Aliases ----

(define-for-syntax (append* v**) (apply append v**))

;; Alias for `vector`; used to construct the mutable vector of
;; alternative thunks in the `amb` macro expansion for clarity.
(define thunks vector)


;; ---- Prompt Tags and Marks ----

;; `amb-prompt-tag` delimits the leaf-labeled tree.  Every `shift-at`,
;; `amb-fail`, `unsafe-fail`, `amb-empty-handler`, and `make-amb-root`
;; communicates through this prompt; the root mark (below) is looked up
;; no further out than it.
(define amb-prompt-tag (make-continuation-prompt-tag 'amb))

;; The frontier new trees belong to is kept as a continuation mark:
;; the root frame of each search (`make-amb-root`) carries an
;; `amb-search` record of the root frontier, fresh for each search
;; (unless the root has the default handler: then no mark is found, a
;; tree records #f for "the root", and none is needed), and `amb/local`
;; carries its own frontier.  Every continuation of the
;; search is captured inside the root frame, so the mark comes back
;; whenever a tree of the search is resumed, and the innermost one (an
;; inner search's root, or a local frontier) is found first.  The record
;; tells two searches apart even when they share their root frontier.
(define frontier-key (make-continuation-mark-key 'amb-frontier))
(struct amb-search (frontier) #:authentic)
(define (marked-frontier)
  (continuation-mark-set-first #f frontier-key #f amb-prompt-tag))
(define (unmark m)
  (if (amb-search? m) (amb-search-frontier m) m))


;; ---- Frontiers ----

;; A frontier is a mutable handle on an immutable deque of items, and
;; the empty handler that answers `(amb)` in it.  The deque operations
;; are not exported.

;;
;; `current` is the stack of items popped whose resumption has not
;; returned yet (see `land!` in the driver).

(struct amb-frontier ([items #:mutable] empty-handler [current #:mutable])
  #:authentic)
(define (new-frontier handler)
  (amb-frontier empty-ideque handler #f))

(define (make-amb-frontier [handler (inherited-handler)])
  (new-frontier handler))

(define (inherited-handler)
  (define f (frontier-now))
  (if f (amb-frontier-empty-handler f) amb-fail))

(define (amb-frontier-length f)
  (unless (amb-frontier? f)
    (raise-argument-error 'amb-frontier-length "amb-frontier?" f))
  (ideque-length (amb-frontier-items f)))

;; ---- Parameters ----

(define current-amb-depth-first? (make-parameter #t  ))
(define current-amb-fair?        (make-parameter #f  ))
(define current-amb-shuffler     (make-parameter void))

;; Anywhere, and inside a search only (where `amb-prompt-tag` is
;; available): in `make-amb-tree` (where `shift-at` needs the prompt
;; anyway) and in `amb-empty-handler` (where the error for `(amb)`
;; outside any search comes from).  #f when no frontier is marked:
;; outside any search, or in the root of a search with the default
;; handler.
(define (frontier-now)
  (and (continuation-prompt-available? amb-prompt-tag)
       (frontier-in-prompt)))
(define (frontier-in-prompt)
  (unmark (marked-frontier)))

;; `amb/local` is `amb` in a fresh frontier of its own; the code after
;; it returns is back in the frontier around it.
(define (call-with-local-frontier thk)
  (with-continuation-mark frontier-key (new-frontier (inherited-handler))
    (thk)))


;; ---- Tree Construction ----

;; An `amb-tree` bundles a suspended continuation with the search
;; strategy parameters and the frontier that were in effect at capture
;; time.  `prop:procedure` makes the struct callable — `(t)` invokes
;; `(k)`, returning the next result in the tree protocol.
(struct amb-tree (k depth-first? fair? frontier)
  #:property prop:procedure (struct-field-index k))

;; A mounted frontier, as an item of another frontier.  `fair?` is
;; that of the tree whose birth mounted it: it decides where the mount
;; goes back after a turn that produced a leaf.
(struct mount (frontier fair?) #:authentic)

;; An item being resumed, on the `current` stack of its frontier:
;; `below` is the rest of the stack, and `inner` the stack of the
;; frontier the item mounts, if it is a mount, when it was entered.
(struct flight (item below inner) #:authentic)

;; `make-amb-tree` snapshots the current parameters and uses
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
  (define frontier (frontier-in-prompt))
  (shift-at amb-prompt-tag k (amb-tree k depth-first? fair? frontier)))

;; `make-amb-root` constructs the root `amb-tree` of a leaf-labeled
;; tree directly.  The `first?` flag produces exactly one leaf on the
;; first invocation and returns `dead` on the second, so the
;; root is naturally discarded from the frontier after its sole result.
;; The root mark is attached here (see `frontier-key`).
(define (make-amb-root thk frontier)
  (define search (amb-search frontier))
  (define first? #t)
  (define depth-first? (current-amb-depth-first?))
  (define fair? (current-amb-fair?))
  (define (k)
    (reset-at amb-prompt-tag
      (cond
        [first?
         (set! first? #f)
         (if (eq? (amb-frontier-empty-handler frontier) amb-fail)
             ;; Not attached for the default handler, which a lookup that
             ;; finds no mark falls back to anyway: a frame with a mark
             ;; makes every resumption of the search slower.
             (call-with-values thk list)
             ;; Not in tail position of `reset-at`: a mark on the
             ;; prompt's own frame can reappear innermost when a
             ;; continuation captured across another prompt is applied
             ;; again (seen on Racket v9.3 CS), which would hide the
             ;; mark of a frontier installed inside this one.
             (let ([vs (with-continuation-mark frontier-key search
                         (call-with-values thk list))])
               vs))]
        [else dead])))
  (amb-tree k depth-first? fair? frontier))


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
;; here.  What it ends *with* is decided by the *empty handler* of the
;; current frontier (the innermost one marked): of the root frontier, the
;; handler given as `#:empty-handler` to `in-amb` / `in-amb/do`
;; (default `amb-fail`); of a local one, the handler it was made with
;; (by default that of the frontier it was made in).
;; `amb-empty-handler` calls it at the point of `(amb)`, in its dynamic
;; context, and outputs the values it returns as one answer of the
;; search; with the default handler, `(amb)` outputs nothing.  Either
;; way `amb-empty-handler` never returns, and the tree being resumed is
;; pushed back (per its `fair?`).
;; For example, (when (amb #t #f) (amb)) is a pause: the search outputs
;; the handler's answer, then continues after it.
;;
;; The handler may reach `(amb)` itself: that is a recursive call of the
;; handler (of whatever frontier is current there), whose `(amb)`
;; abandons the outer call, so the answer is that of the innermost call.
;; It ends only if the handler changes some state; to avoid it, the
;; handler can install a frontier with another handler.
(define (amb-empty-handler)
  (define f (frontier-in-prompt))
  (define handler (if f (amb-frontier-empty-handler f) amb-fail))
  (cond
    [(eq? handler amb-fail) (abort/cc amb-prompt-tag yield-thunk)]
    [else
     (define vs (call-with-values handler list))
     (abort/cc amb-prompt-tag (λ () (yielded vs)))]))
;; `(amb)` is reported to the driver as a *yield*: the handler's values,
;; or #f under the default handler.  In the root frontier the driver
;; treats a yield as a leaf with those values, or as nothing produced,
;; which is what `(amb-fail)` is.  A separate result lets a local
;; frontier tell `(amb)` from both: there it also ends a turn.
(struct yielded (vs))
(define yield-nothing (yielded #f))
(define (yield-thunk) yield-nothing)


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


;; ---- Local Choices ----

;; `amb/local` is `amb` in a fresh frontier.
(define-syntax (amb/local stx)
  (syntax-parse stx
    [(_ expr* ...)
     (syntax/loc stx (call-with-local-frontier (λ () (amb expr* ...))))]))


;; ---- Search Driver ----
;;
;; `in-amb*` and `in-amb*/do` convert a nondeterministic computation
;; into a lazy stream or a do-sequence respectively.

(define-values (in-amb* in-amb*/do)
  (let ()
    (define (push-front! f v)
      (set-amb-frontier-items! f (ideque-add-front (amb-frontier-items f) v)))
    (define (push-back! f v)
      (set-amb-frontier-items! f (ideque-add-back (amb-frontier-items f) v)))
    ;; `search` runs the frontier `f` until a leaf is found or `f` is
    ;; empty.  `running` lists the frontiers being run, innermost first
    ;; (so its head is `f`).  For each item `item0` popped from `f`, a
    ;; tree is resumed with `(item0)`, and a mount by running its
    ;; frontier with this same function (`root?` #f), which stops at a
    ;; leaf or at a yield and passes it on, so the result of a mount is
    ;; never a branch.  Then the result `tree1` is inspected:
    ;;
    ;;   dead      → dead end; discard `item0` and continue searching
    ;;   #f        → nothing produced; push `item0` back (per its
    ;;               `fair?`) and continue searching
    ;;   list?     → leaf; push `item0` back (per its `fair?`), return
    ;;               the result and suspend searching
    ;;   amb-tree? → branch; push `item0` back (per its `fair?`), place
    ;;               `tree1` (per `item0`'s `depth-first?`, see `place!`),
    ;;               and continue searching
    ;;   yielded?  → yield; push `item0` back, to the back if it takes
    ;;               turns (see below) and per its `fair?` otherwise;
    ;;               then, at the root, return the values it carries as a
    ;;               leaf, or continue searching if it carries none
    ;;
    ;; On a yield, an item takes turns if it is a mount or if `f` is not
    ;; the root.  A tree in the root frontier does not: it follows its
    ;; `fair?` field, as it does for a leaf.
    ;;
    ;; `f` is a mutable handle, so it is read again after every
    ;; resumption: the code resumed may have run a search of its own on
    ;; the same frontier.
    ;;
    ;; While an item is resumed it is pushed on the `current` stack of
    ;; `f` (a `flight`, which also records the stack of the frontier it
    ;; mounts, if any).  If the resumption escapes (the empty handler
    ;; raises, say), the next step of the search puts back, in front of
    ;; their frontiers, every item pushed since its own step began, and
    ;; so on down the frontiers they mount (`land!`), so that the search
    ;; can go on.  A search started inside on the same frontier pushes
    ;; above the outer one's item; if it escapes, the outer one puts back
    ;; both, and if it is stepped again itself it puts back only its own.
    (define (search f running root?)
      (define items (amb-frontier-items f))
      (cond
        [(ideque-empty? items) #f]
        [else
         (define-values (item0 rest) (ideque-pop-front items))
         (set-amb-frontier-items! f rest)
         (define mounted? (mount? item0))
         (define prev (amb-frontier-current f))
         (set-amb-frontier-current!
          f (flight item0 prev
                    (and mounted? (amb-frontier-current (mount-frontier item0)))))
         (define tree1
           (cond
             [(not mounted?) (item0)]
             [else
              (define g (mount-frontier item0))
              ;; a frontier already being run (it was mounted in a
              ;; frontier of its own) has nothing to give here
              (if (memq g running)
                  dead
                  (or (search g (cons g running) #f) dead))]))
         (set-amb-frontier-current! f prev)
         (define fair? (if mounted? (mount-fair? item0) (amb-tree-fair? item0)))
         (cond
           [(eq? tree1 dead) (search f running root?)]
           [(amb-tree? tree1)
            (if fair? (push-back! f item0) (push-front! f item0))
            (place! f running tree1 (amb-tree-depth-first? item0))
            (search f running root?)]
           [(yielded? tree1)
            (if (or mounted? (not root?) fair?) (push-back! f item0) (push-front! f item0))
            (cond
              [(not root?) tree1]
              [(yielded-vs tree1) => values]
              [else (search f running root?)])]
           [else
            (if fair? (push-back! f item0) (push-front! f item0))
            (cond
              [(or (pair? tree1) (null? tree1)) tree1]
              [(not tree1) (search f running root?)]
              [else (error 'amb "internal: unexpected tree result ~e" tree1)])])]))
    (define (land! f base)
      (let loop ([fl (amb-frontier-current f)])
        (unless (or (eq? fl base) (not fl))
          (define item (flight-item fl))
          (push-front! f item)
          (when (mount? item) (land! (mount-frontier item) (flight-inner fl)))
          (loop (flight-below fl))))
      (set-amb-frontier-current! f base))
    ;; A child born in a frontier that is not being run goes into that
    ;; frontier, which is mounted in `f`; otherwise it goes into `f`
    ;; itself.  Both go to the front under `depth-first?`, to the back
    ;; otherwise.
    (define (place! f running tree1 depth-first?)
      (define push! (if depth-first? push-front! push-back!))
      ;; #f: born in the root of this search, which is being run
      (define g (amb-tree-frontier tree1))
      (cond
        [(or (not g) (eq? g f) (memq g running)) (push! f tree1)]
        [else
         (push! g tree1)
         (push! f (mount g (amb-tree-fair? tree1)))]))
    (define ((make make-sequence empty-sequence raise-empty-sequence-error)
             thk [handler amb-fail] #:frontier [root #f])
      (if (equal? thk amb-fail)
          empty-sequence
          (let ([loop #f])
            (make-sequence
             ;; Set up once: if the first step escapes, the sequence calls
             ;; this again, which must not push another root.
             (λ ()
               (unless loop
                 (when (and root (not (eq? handler amb-fail)))
                   (raise-arguments-error
                    'in-amb "an empty handler and a frontier cannot both be given"
                    "handler" handler "frontier" root))
                 (define f (or root (new-frontier handler)))
                 (push-front! f (make-amb-root thk f))
                 (define running (list f))
                 ;; the stack of `f` when the current step began; #t
                 ;; while no step is running
                 (define base #t)
                 (set! loop
                       (λ ()
                         ;; still in a step: the last one escaped
                         (unless (eq? base #t) (land! f base))
                         (set! base (amb-frontier-current f))
                         (define v* (search f running #t))
                         (set! base #t)
                         (values v* (if v* loop raise-empty-sequence-error)))))
               (loop))))))
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
           (#,derived-stx (λ () expr) handler))]
        [(_ expr #:frontier frontier)
         (quasisyntax/loc stx
           (#,derived-stx (λ () expr) #:frontier frontier))]))
    (values (make #'in-amb*)
            (make #'in-amb*/do))))
