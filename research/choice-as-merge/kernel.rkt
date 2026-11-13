#lang racket/base

;; ===================================================================
;; Choice as merge: a search is a fold of the computation into a stream
;; ===================================================================
;;
;; A direct-style nondeterministic computation is a tree that is revealed
;; one node at a time: it returns (a leaf), fails, pauses, or chooses.
;; An observer of a search sees a sequence of events over time: an answer,
;; or "not yet" (a pause).  So a search is a function from the tree to
;;
;;   Stream = '()                     ; no more events
;;          | (cons vs (→ Stream))    ; an answer, then (lazily) the rest
;;          | (→ Stream)              ; a pause, then the rest
;;
;; and every operation says what it contributes to that stream, in terms
;; of the stream `(k v)` of the rest of the computation:
;;
;;   return vs   ↦  the stream of the one answer vs
;;   fail        ↦  '()
;;   pause       ↦  a pause, then (k)
;;   choose a b  ↦  (merge (k a) (λ () (k b)))
;;
;; A search strategy is the `merge`: a binary operation on streams.  The
;; frontier of the search is not a data structure; it is the part of the
;; stream expression that has not been evaluated yet.

(require racket/control
         "catenable.rkt")

(provide
 ;; effects
 amb amb* amb-fail amb-pause current-amb-merge
 ;; a local search, as a value, and back
 amb-reify amb-reflect amb-await amb-local amb-once
 ;; strategies
 amb-append amb-interleave amb-alternate amb-level
 ;; observing a stream
 in-amb in-answers amb-take amb-events
 amb-from)


;; ---- Streams ----

;; The stream of one answer.  Its rest is the thunk `done`, which
;; `amb-reflect` recognizes: a stream known to end after its first answer
;; (the case `(a)` of the first edition's streams).
(define (done) '())
(define (unit vs) (cons vs done))


;; ---- Effects ----

(define tag (make-continuation-prompt-tag 'amb))

;; The stream of the events of (thk), which runs as a search of its own:
;; no effect of (thk) reaches the enclosing search, and nothing runs after
;; an event until the stream is forced further.
(define (amb-reify thk)
  (reset-at tag (unit (call-with-values thk list))))

;; In the three effects, `k` is the rest of the computation, up to the
;; search: (k v) is the stream of the events of the rest when the
;; operation returns v.

(define (amb-fail)
  (shift-at tag k '()))

(define (amb-pause)
  (shift-at tag k (λ () (k (void)))))

;; A choice among the thunks `alts`, merged from the right:
;;   (merge (k a) (λ () (merge (k b) (λ () (k c)))))
;; The chosen thunk is called where the choice was made, so a choice made
;; inside an alternative is a choice of the same search.
(define (amb* merge alts)
  ((shift-at tag k
     (let loop ([alts alts])
       (cond
         [(null? alts) '()]
         [(null? (cdr alts)) (k (car alts))]
         [else (merge (k (car alts)) (λ () (loop (cdr alts))))])))))


;; ---- Strategies ----
;;
;; (merge s t) merges the stream s with the stream that the thunk t
;; produces; t is forced only when the merge turns to it.
;;
;; The strategies agree on answers that are ready and on a stream that
;; has ended,
;;
;;   '()      ∨ t  =  t
;;   (a : s)  ∨ t  =  a : (s ∨ t)        (amb-alternate: a : (t ∨ s))
;;
;; and differ in one equation, which says what a pause of s means:
;;
;;   amb-append       pause s ∨ t        =  pause (s ∨ t)     s keeps the turn
;;   amb-interleave   pause s ∨ t        =  pause (t ∨ s)     the turn passes on
;;   amb-level        pause s ∨ pause t  =  pause (s ∨ t)     both wait together
;;
;; A pause and a lazy rest are thunks, and so is t: to pass the turn on
;; is to exchange the two arguments.
;;
;; These equations are the definitions; model.rkt has all four as code.
;; amb-interleave and amb-alternate below are their equations.
;; amb-append and amb-level are associative, and are written to use it
;; (see "Associative strategies").

;; The interleaving of miniKanren (microKanren's `mplus`, the second
;; edition's `append-inf`): the turn passes on at each pause.
(define (amb-interleave s t)
  (cond
    [(null? s) (t)]
    [(pair? s) (cons (car s) (λ () (amb-interleave ((cdr s)) t)))]
    [else (λ () (amb-interleave (t) s))]))

;; The turn passes on at each event, an answer too (the first edition's
;; `mplusi`, which has no pauses).
(define (amb-alternate s t)
  (cond
    [(null? s) (t)]
    [(pair? s) (cons (car s) (λ () (amb-alternate (t) (cdr s))))]
    [else (λ () (amb-alternate (t) s))]))


;; ---- Associative strategies ----
;;
;; Written from its equations, a merge is a closure around s, and an event
;; of s passes through one closure for every choice it is under: n nested
;; choices cost n steps for each answer.  For an associative merge,
;;
;;   (s ∨ t) ∨ u  =  s ∨ (t ∨ u)
;;
;; the grouping does not matter, so a stream that is not finished can be a
;; record of what is left to do.  When the thunk that a record is to run
;; next is a record of the same strategy, the two are one record: what is
;; left of the inner one, and then what is left of the outer one.
;;
;; A record is a thunk, like every stream that waits, so a strategy that
;; looks at its arguments only by their events (all of them do) needs to
;; know nothing about it.
;;
;; The equation says that the streams are the same.  That a record runs
;; the thunks it was given when the equations do, and no sooner, is a
;; second property, and how many steps it takes is a third; see README.
;;
;; What is left to do is kept in lists that are appended in constant time
;; (catenable.rkt), because either of the two lists can be the long one.

;; Depth-first search: all of s, then t.
;;
;;   (appended s ts)   the stream (s), then the streams of the thunks ts
(struct appended (s ts)
  #:property prop:procedure
  (λ (self) (append-run (appended-s self) (appended-ts self))))

(define (amb-append s t)
  (cond
    [(null? s) (t)]
    [(pair? s) (cons (car s) (appended (cdr s) (cat-of t)))]
    [else (appended s (cat-of t))]))

(define (append-run s ts)
  (if (appended? s)
      (append-run (appended-s s) (cat-append (appended-ts s) ts))
      (let loop ([s (s)] [ts ts])
        (cond
          [(pair? s) (cons (car s) (appended (cdr s) ts))]
          [(procedure? s) (appended s ts)]
          [(null? ts) '()]
          [else (loop ((cat-first ts)) (cat-rest ts))]))))

;; Breadth-first search by pauses: when s pauses, t runs up to its own
;; pause, and then the merged stream pauses once.  The merged stream is a
;; sequence of rounds; in a round, each stream runs up to its pause.
;;
;;   (leveled ws s ts)   the rest of a round: the thunks ws have paused
;;                       in it, (s) is running, and the thunks ts have
;;                       yet to run; then one pause, and the next round,
;;                       in which they run in the same order
(struct leveled (ws s ts)
  #:property prop:procedure
  (λ (self) (level-run (leveled-ws self) (leveled-s self) (leveled-ts self))))

(define (amb-level s t)
  (cond
    [(null? s) (t)]
    [(pair? s) (cons (car s) (leveled '() (cdr s) (cat-of t)))]
    [else (level-run (cat-of s) t '())]))

(define (level-run ws s ts)
  (if (leveled? s)
      (level-run (cat-append ws (leveled-ws s))
                 (leveled-s s)
                 (cat-append (leveled-ts s) ts))
      (let ([s (s)])
        (cond
          [(pair? s) (cons (car s) (leveled ws (cdr s) ts))]
          [(null? s) (level-next ws ts)]
          [else (level-next (cat-append ws (cat-of s)) ts)]))))

;; The next stream of the round; or the end of the round: the pause, and
;; then the streams that have paused.
(define (level-next ws ts)
  (cond
    [(not (null? ts)) (level-run ws (cat-first ts) (cat-rest ts))]
    [(null? ws) '()]
    [else
     (let ([s (cat-first ws)] [ws (cat-rest ws)])
       (if (null? ws)
           s
           (leveled '() s ws)))]))

;; The strategy belongs to the choice point: `amb` reads it when the
;; choice is made.
(define current-amb-merge (make-parameter amb-append))

(define-syntax-rule (amb e ...)
  (amb* (current-amb-merge) (list (λ () e) ...)))


;; ---- From a stream back to the computation ----

;; Passes the pauses at the front of `s` on to the enclosing search, and
;; returns the rest: '() or a stream that starts with an answer.
(define (amb-await s)
  (cond
    [(procedure? s) (amb-pause) (amb-await (s))]
    [else s]))

;; Returns an answer of `s`, as a choice of the enclosing search among
;; all of them; the pauses of `s` are pauses of the enclosing search.
;; This is the `bind` of the stream: each answer of `s` is followed by
;; the rest of the computation.
(define (amb-reflect s [merge (current-amb-merge)])
  (let ([s (amb-await s)])
    (cond
      [(null? s) (amb-fail)]
      [(eq? (cdr s) done) (apply values (car s))]
      [else
       (amb* merge
             (list (λ () (apply values (car s)))
                   (λ () (amb-reflect ((cdr s)) merge))))])))

;; (amb-local e) has the answers of e, searched for on their own: the
;; choices of e are merged with each other before the rest of the
;; computation sees any of them.
(define-syntax-rule (amb-local e)
  (amb-reflect (amb-reify (λ () e))))

;; The first answer of (thk), or failure.
(define (amb-once thk)
  (let ([s (amb-await (amb-reify thk))])
    (if (null? s)
        (amb-fail)
        (apply values (car s)))))

;; An element of the sequence: one choice among all of them, merged from
;; the right as in `amb*`, and generated as the search goes on.
(define (amb-from seq [merge (current-amb-merge)])
  (define-values (more? get) (sequence-generate seq))
  (shift-at tag k
    (let loop ()
      (if (more?)
          (let ([v (get)]) (merge (k v) loop))
          '()))))


;; ---- Observing a stream ----

;; The answers of a stream, as a single-pass sequence; pauses are skipped.
(define (in-answers s)
  (make-do-sequence
   (λ ()
     (define (more? _)
       (let loop ()
         (when (procedure? s) (set! s (s)) (loop)))
       (pair? s))
     (define (get _)
       (define vs (car s))
       (set! s (cdr s))
       (apply values vs))
     (values get void (void) more? #f #f))))

(define-syntax-rule (in-amb e)
  (in-answers (amb-reify (λ () e))))

;; The first n answers (all of them when n is #f), one value each.
(define (amb-take n s)
  (for/list ([_ (in-range (or n +inf.0))]
             [v (in-answers s)])
    v))

;; The first `fuel` events: each answer (one value), or 'pause.  Nothing
;; is forced after the last of them.
(define (amb-events s [fuel +inf.0])
  (let loop ([s s] [fuel fuel])
    (cond
      [(or (null? s) (<= fuel 0)) '()]
      [(pair? s)
       (cons (caar s) (if (<= fuel 1) '() (loop ((cdr s)) (sub1 fuel))))]
      [else
       (cons 'pause (if (<= fuel 1) '() (loop (s) (sub1 fuel))))])))
