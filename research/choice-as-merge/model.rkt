#lang racket/base

;; ===================================================================
;; A small goal language with three interpretations
;; ===================================================================
;;
;; Goals transform a state (here: the list of the tags passed so far), as
;; in ambKanren, where Goal = Substitution → Substitution.
;;
;;   g ::= (tag x)        succeed, adding x to the state
;;       | succeed
;;       | fail
;;       | pause          pause, then succeed
;;       | (seq g g)      conjunction
;;       | (alt g g)      disjunction
;;       | (local g)      g, as a local search
;;       | (once g)       the first answer of g
;;       | (ifte g g g)   committed choice
;;
;;   direct   runs a goal in direct style, with the effects of kernel.rkt.
;;   spec     the same without control operators: every goal takes the
;;            rest of the computation as an explicit function `k`.  This
;;            is the specification of the kernel.
;;   book     goals return streams, and conjunction is the `bind` of the
;;            stream, as in microKanren and The Reasoned Schemer (2nd
;;            ed.); `merge` plays the role of `mplus` / `append-inf`.
;;
;; `trs2` is the code of the second edition itself (append-inf and
;; append-map-inf, with eager rests), to check `book` against.

(require racket/match
         "kernel.rkt")

(provide direct spec book trs2
         events answers
         random-goal goal-size)

;; A stream of events as a list: states, and 'pause.
(define (events s) (amb-events s))
(define (answers s) (filter pair? (events s)))

(define (done) '())
(define (unit a) (cons (list a) done))


;; ---- direct: the kernel ----

(define ((direct merge) g)
  (define (run g s)
    (match g
      [`(tag ,x) (cons x s)]
      ['succeed s]
      ['fail (amb-fail)]
      ['pause (amb-pause) s]
      [`(seq ,g1 ,g2) (run g2 (run g1 s))]
      [`(alt ,g1 ,g2) (amb (run g1 s) (run g2 s))]
      [`(local ,g) (amb-local (run g s))]
      [`(once ,g) (amb-once (λ () (run g s)))]
      [`(ifte ,g1 ,g2 ,g3)
       (define s* (amb-await (amb-reify (λ () (run g1 s)))))
       (if (null? s*)
           (run g3 s)
           (run g2 (amb-reflect s*)))]))
  (parameterize ([current-amb-merge merge])
    (events (amb-reify (λ () (run g '(start)))))))


;; ---- spec: the same, with the rest of the computation made explicit ----

(define ((spec merge) g)
  ;; (run g s k) is the stream of the events of g in state s, followed
  ;; by the rest of the computation k : State → Stream.
  (define (run g s k)
    (match g
      [`(tag ,x) (k (cons x s))]
      ['succeed (k s)]
      ['fail '()]
      ['pause (λ () (k s))]
      [`(seq ,g1 ,g2) (run g1 s (λ (s) (run g2 s k)))]
      [`(alt ,g1 ,g2) (merge (run g1 s k) (λ () (run g2 s k)))]
      [`(local ,g) (reflect (run g s unit) k)]
      [`(once ,g)
       (await (run g s unit) (λ (s*) (if (null? s*) '() (k (caar s*)))))]
      [`(ifte ,g1 ,g2 ,g3)
       (await (run g1 s unit)
              (λ (s*)
                (if (null? s*)
                    (run g3 s k)
                    (reflect s* (λ (s) (run g2 s k))))))]))
  ;; the pauses at the front of s*, then (k s*) for the rest
  (define (await s* k)
    (if (procedure? s*)
        (λ () (await (s*) k))
        (k s*)))
  ;; the stream s*, each answer followed by the rest of the computation
  (define (reflect s* k)
    (await s*
           (λ (s*)
             (cond
               [(null? s*) '()]
               [(eq? (cdr s*) done) (k (caar s*))]
               [else (merge (k (caar s*))
                            (λ () (reflect ((cdr s*)) k)))]))))
  (events (run g '(start) unit)))


;; ---- book: goals return streams; conjunction is bind ----

(define ((book merge) g)
  (define (run g s)
    (match g
      [`(tag ,x) (unit (cons x s))]
      ['succeed (unit s)]
      ['fail '()]
      ['pause (λ () (unit s))]
      [`(seq ,g1 ,g2) (bind (run g1 s) (λ (s) (run g2 s)))]
      [`(alt ,g1 ,g2) (merge (run g1 s) (λ () (run g2 s)))]
      [`(local ,g) (run g s)]
      [`(once ,g)
       (let loop ([s* (run g s)])
         (cond
           [(null? s*) '()]
           [(pair? s*) (unit (caar s*))]
           [else (λ () (loop (s*)))]))]
      [`(ifte ,g1 ,g2 ,g3)
       (let loop ([s* (run g1 s)])
         (cond
           [(null? s*) (run g3 s)]
           [(pair? s*) (bind s* (λ (s) (run g2 s)))]
           [else (λ () (loop (s*)))]))]))
  (define (bind s* f)
    (cond
      [(null? s*) '()]
      [(pair? s*)
       (if (eq? (cdr s*) done)
           (f (caar s*))
           (merge (f (caar s*)) (λ () (bind ((cdr s*)) f))))]
      [else (λ () (bind (s*) f))]))
  (events (run g '(start))))


;; ---- trs2: The Reasoned Schemer, 2nd ed., trs2-impl.scm ----

(define (trs2 g)
  (define (append-inf s-inf t-inf)
    (cond
      [(null? s-inf) t-inf]
      [(pair? s-inf) (cons (car s-inf) (append-inf (cdr s-inf) t-inf))]
      [else (λ () (append-inf t-inf (s-inf)))]))
  (define (append-map-inf g s-inf)
    (cond
      [(null? s-inf) '()]
      [(pair? s-inf) (append-inf (g (car s-inf)) (append-map-inf g (cdr s-inf)))]
      [else (λ () (append-map-inf g (s-inf)))]))
  (define (run g s)
    (match g
      [`(tag ,x) (list (cons x s))]
      ['succeed (list s)]
      ['fail '()]
      ['pause (λ () (list s))]
      [`(seq ,g1 ,g2) (append-map-inf (λ (s) (run g2 s)) (run g1 s))]
      [`(alt ,g1 ,g2) (append-inf (run g1 s) (run g2 s))]
      [`(local ,g) (run g s)]
      [`(once ,g)
       (let loop ([s-inf (run g s)])
         (cond
           [(null? s-inf) '()]
           [(pair? s-inf) (list (car s-inf))]
           [else (λ () (loop (s-inf)))]))]
      [`(ifte ,g1 ,g2 ,g3)
       (let loop ([s-inf (run g1 s)])
         (cond
           [(null? s-inf) (run g3 s)]
           [(pair? s-inf) (append-map-inf (λ (s) (run g2 s)) s-inf)]
           [else (λ () (loop (s-inf)))]))]))
  (let loop ([s-inf (run g '(start))])
    (cond
      [(null? s-inf) '()]
      [(pair? s-inf) (cons (car s-inf) (loop (cdr s-inf)))]
      [else (cons 'pause (loop (s-inf)))])))


;; ---- random goals ----

;; A goal with `size` leaves; every (tag x) gets a fresh x, so an answer
;; is the path that produced it.  `forms` lists the compound forms to use.
(define (random-goal size [forms '(seq alt)] [first-tag 1])
  (define n (sub1 first-tag))
  (define (leaf)
    (case (random 6)
      [(0) 'fail]
      [(1 2) 'pause]
      [else (set! n (add1 n)) `(tag ,n)]))
  (let gen ([size size])
    (cond
      [(<= size 1) (leaf)]
      [else
       (define form (list-ref forms (random (length forms))))
       (case form
         [(seq alt)
          (define l (add1 (random (sub1 size))))
          (list form (gen l) (gen (- size l)))]
         [(local once) (list form (gen (sub1 size)))]
         [(ifte)
          (cond
            [(< size 3) (gen size)]
            [else
             (define l (add1 (random (- size 2))))
             (define m (add1 (random (- size l 1))))
             (list 'ifte (gen l) (gen m) (gen (- size l m)))])])])))

(define (goal-size g)
  (match g
    [(list _ g* ...) (apply + 1 (map goal-size g*))]
    [_ 1]))
