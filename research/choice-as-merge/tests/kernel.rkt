#lang racket/base

;; The kernel by example: effects, strategies, and local searches.

(require rackunit
         "../kernel.rkt")

(define-syntax-rule (events e) (amb-events (amb-reify (λ () e))))
(define-syntax-rule (events/fuel n e) (amb-events (amb-reify (λ () e)) n))
(define-syntax-rule (with merge e ...) (parameterize ([current-amb-merge merge]) e ...))

(define strategies (list amb-append amb-interleave amb-alternate amb-level))

(test-case "choice and failure"
  (check-equal? (events (amb 1 (amb 2 3) (amb 4 5 6) 7 8)) '(1 2 3 4 5 6 7 8))
  (check-equal? (events (let ([x (amb 1 2)] [y (amb 'a 'b)]) (list x y)))
                '((1 a) (1 b) (2 a) (2 b)))
  (check-equal? (events (let ([x (amb 1 2 3 4)]) (when (odd? x) (amb)) x)) '(2 4))
  (check-equal? (events (amb)) '())
  (check-equal? (events (amb-fail)) '())
  ;; a failed alternative does not end the choice it belongs to
  (check-equal? (events (amb (amb) 5)) '(5))
  ;; several values
  (check-equal? (for/list ([(x y) (in-amb (amb (values 1 2) (values 3 4)))]) (+ x y))
                '(3 7)))

(test-case "a pause is an event of the search"
  (check-equal? (events (begin (amb-pause) 1)) '(pause 1))
  (check-equal? (events (amb (begin (amb-pause) 1) 2)) '(pause 1 2))
  ;; in-amb and amb-take show the answers only
  (check-equal? (for/list ([x (in-amb (amb (begin (amb-pause) 1) 2))]) x) '(1 2)))

(test-case "the search is lazy"
  ;; an answer is delivered before the search goes on
  (define (forever) (forever))
  (check-equal? (for/first ([x (in-amb (amb 1 (forever)))]) x) 1)
  ;; and nothing runs after a pause until the stream is forced
  (define ran '())
  (define s (amb-reify (λ () (set! ran (cons 'a ran)) (amb-pause) (set! ran (cons 'b ran)) 1)))
  (check-equal? ran '(a))
  (check-equal? (amb-take 1 s) '(1))
  (check-equal? ran '(b a))
  ;; a bound that is checked first stops the search
  (let ([n 0])
    (for ([_ 2] [x (in-amb (begin0 (amb 1 2 3) (set! n (add1 n))))]) x)
    (check-equal? n 2))
  ;; a choice among infinitely many alternatives
  (check-equal? (for/list ([_ 4] [x (in-amb (amb-from (in-naturals)))]) x) '(0 1 2 3)))

;; 0, 1, 2, ... with a pause before each recursive call
(define (count n) (amb n (begin (amb-pause) (count (add1 n)))))
(define (nevero) (amb-pause) (nevero))

(test-case "the strategies: what a pause means to a choice"
  (define-syntax-rule (trace merge)
    (with merge (events/fuel 12 (amb (list 'l (count 0)) (list 'r (count 0))))))
  ;; the left alternative keeps the turn
  (check-equal? (trace amb-append)
                '((l 0) pause (l 1) pause (l 2) pause (l 3) pause (l 4) pause (l 5) pause))
  ;; the turn passes on at each pause
  (check-equal? (trace amb-interleave)
                '((l 0) pause (r 0) pause (l 1) pause (r 1) pause (l 2) pause (r 2) pause))
  ;; the turn passes on at each event
  (check-equal? (trace amb-alternate)
                '((l 0) (r 0) pause pause (l 1) (r 1) pause pause (l 2) (r 2) pause pause))
  ;; both pause together
  (check-equal? (trace amb-level)
                '((l 0) (r 0) pause (l 1) (r 1) pause (l 2) (r 2) pause (l 3) (r 3) pause)))

(test-case "the strategy belongs to the choice point"
  ;; an interleaving choice whose alternatives choose depth-first
  (check-equal?
   (with amb-interleave
     (events/fuel 9 (amb (with amb-append (amb (list 'a (count 0)) 'never))
                         (list 'b (count 0)))))
   '((a 0) pause (b 0) pause (a 1) pause (b 1) pause (a 2))))

(define (str . es)                      ; a stream from its events
  (cond [(null? es) '()]
        [(eq? (car es) 'pause) (λ () (apply str (cdr es)))]
        [else (cons (list (car es)) (λ () (apply str (cdr es))))]))

(test-case "the defining equations"
  (define (merged merge l r) (amb-events (merge (apply str l) (λ () (apply str r)))))
  (for ([merge strategies])
    (check-equal? (merged merge '() '(pause x y)) '(pause x y))
    (check-equal? (merged merge '(pause x y) '()) '(pause x y)))
  (check-equal? (merged amb-append     '(a pause b) '(x pause y)) '(a pause b x pause y))
  (check-equal? (merged amb-interleave '(a pause b) '(x pause y)) '(a pause x pause b y))
  (check-equal? (merged amb-alternate  '(a pause b) '(x pause y)) '(a x pause pause b y))
  (check-equal? (merged amb-level      '(a pause b) '(x pause y)) '(a x pause b y)))

(test-case "the grouping shows, unless the strategy is associative"
  ;; (s ∨ t) ∨ u and s ∨ (t ∨ u), for the streams of the events s, t, u
  (define (groupings merge s t u)
    (list (amb-events (merge (merge (apply str s) (λ () (apply str t)))
                             (λ () (apply str u))))
          (amb-events (merge (apply str s)
                             (λ () (merge (apply str t) (λ () (apply str u))))))))
  ;; the two examples of Astra's review (#22)
  (check-equal? (groupings amb-interleave '(pause) '(b) '(c)) '((pause c b) (pause b c)))
  (check-equal? (groupings amb-alternate '(a) '(b) '(c)) '((a c b) (a b c)))
  (check-equal? (groupings amb-append '(pause) '(b) '(c)) '((pause b c) (pause b c)))
  (check-equal? (groupings amb-level '(pause) '(b) '(c)) '((b c pause) (b c pause)))
  (for ([merge (list amb-append amb-level)])
    (check-equal? (groupings merge '(a) '(b) '(c)) '((a b c) (a b c))))
  ;; a round of amb-level that has no answers is still a round
  (check-equal? (groupings amb-level '(pause) '(pause pause b) '(c pause))
                '((c pause pause b) (c pause pause b)))
  ;; the same answers in another order are other answers to an observer
  ;; that takes the first one
  (with amb-interleave
    (check-equal? (events (amb-once (λ () (amb (amb (begin (amb-pause) (amb)) 'b) 'c))))
                  '(pause c))
    (check-equal? (events (amb-once (λ () (amb (begin (amb-pause) (amb)) (amb 'b 'c)))))
                  '(pause b))))

(test-case "no alternatives is failure; a pause is not"
  (check-equal? (events (begin (amb) 'x)) '())
  (check-equal? (events (begin (amb-pause) 'x)) '(pause x))
  (for ([merge strategies])
    (with merge
      (check-equal? (events (amb (amb) (begin (amb-pause) 'x))) '(pause x))
      (check-equal? (events (amb (begin (amb-pause) 'x) (amb))) '(pause x)))))

(test-case "turns among three alternatives"
  ;; how many of the first 1200 answers come from each alternative
  (define (shares merge)
    (define es
      (with merge
        (filter pair? (events/fuel 2400 (amb (list 'a (count 0)) (list 'b (count 0)) (list 'c (count 0)))))))
    (for/list ([x '(a b c)])
      (/ (for/sum ([e es]) (if (eq? (car e) x) 1 0)) (length es))))
  (check-equal? (shares amb-append) '(1 0 0))
  ;; a choice among three is a choice between the first and the choice
  ;; between the other two
  (check-equal? (shares amb-interleave) '(1/2 1/4 1/4))
  (check-equal? (shares amb-alternate) '(1/2 1/4 1/4))
  ;; amb-level runs every stream up to its pause in each round, and
  ;; each of these three has one answer in a round
  (check-equal? (shares amb-level) '(1/3 1/3 1/3))
  ;; with two answers in a round for the first, and one for the others,
  ;; the shares of the answers are those of the answers in a round:
  ;; associativity says that the grouping does not show, and no more
  (define (count2 n) (amb n n (begin (amb-pause) (count2 (add1 n)))))
  (define es
    (with amb-level
      (filter pair? (events/fuel 2500 (amb (list 'a (count2 0)) (list 'b (count 0)) (list 'c (count 0)))))))
  (check-equal? (for/list ([x '(a b c)])
                  (/ (for/sum ([e es]) (if (eq? (car e) x) 1 0)) (length es)))
                '(1/2 1/4 1/4)))

(test-case "a local search is a value"
  ;; the choices of the local search are not choices of the enclosing one
  (check-equal? (events (list (amb 1 2) (amb-events (amb-reify (λ () (amb 'a 'b))))))
                '((1 (a b)) (2 (a b))))
  ;; a search that does not ask for the pauses of a local search still
  ;; gets every answer
  (check-equal? (events (for/list ([x (in-amb (amb (begin (amb-pause) 1) 2))]) x))
                '((1 2)))
  ;; reflect gives the events back
  (for ([merge strategies])
    (with merge
      (check-equal? (events (amb-reflect (amb-reify (λ () (amb (begin (amb-pause) 1) 2 (begin (amb-pause) 3))))))
                    (events (amb (begin (amb-pause) 1) 2 (begin (amb-pause) 3)))))))

(test-case "the pauses of a local search can be passed on"
  ;; amb-once looks for one answer, and gives the other alternatives of
  ;; the enclosing choice their turn while it does
  (check-equal? (with amb-interleave (amb-take 1 (amb-reify (λ () (amb (amb-once nevero) 1)))))
                '(1))
  (check-equal? (with amb-interleave (events/fuel 4 (amb (amb-once (λ () (count 0))) 'x)))
                '(0 x))
  (check-equal? (with amb-interleave
                  (events (amb (amb-once (λ () (amb-pause) (amb-pause) (amb 1 2))) 'x)))
                '(pause x pause 1))
  ;; a search that keeps the pauses to itself is one step of the
  ;; enclosing search
  (check-equal? (with amb-interleave
                  (events (amb (for/first ([v (in-amb (begin (amb-pause) (amb-pause) (amb 1 2)))]) v) 'x)))
                '(1 x)))

(test-case "amb-local: the same answers, searched for on their own"
  ;; the rest of the computation pauses for a, and not for b and c
  (define (then v) (if (eq? v 'a) (begin (amb-pause) 'A) (if (eq? v 'b) 'B 'C)))
  (with amb-interleave
    ;; The rest of the computation is part of each alternative: when
    ;; (then 'a) pauses, the choice between a and b passes the turn on,
    ;; and so does the choice between the two of them and c.
    (check-equal? (events (then (amb (amb 'a 'b) 'c)))
                  '(pause C B A))
    ;; Chosen locally, the answers a, b, c have lost their grouping
    ;; before the rest of the computation runs: they are a choice among
    ;; three, a | (b | c).
    (check-equal? (events (then (amb-local (amb (amb 'a 'b) 'c))))
                  '(pause B C A))
    (check-equal? (events (then (amb 'a (amb 'b 'c))))
                  '(pause B C A))))

;; ---- the observers force no more than they return ----

(define (never) (error "tail forced"))

(test-case "amb-events stops at the last event it was asked for"
  ;; the last event is an answer: its rest is not forced
  (check-equal? (amb-events (cons '(a) never) 1) '(a))
  (check-equal? (amb-events (cons '(a) (λ () (cons '(b) never))) 2) '(a b))
  ;; the last event is a pause: the stream is not resumed
  (check-equal? (amb-events (λ () (never)) 1) '(pause))
  (check-equal? (amb-events (cons '(a) (λ () never)) 2) '(a pause))
  ;; nothing was asked for
  (check-equal? (amb-events (cons '(a) never) 0) '())
  ;; with one more event asked for, the tail is forced
  (check-exn #rx"tail forced" (λ () (amb-events (cons '(a) never) 2)))
  (check-exn #rx"tail forced" (λ () (amb-events (λ () (never)) 2))))

(test-case "amb-take stops at the last answer it was asked for"
  (check-equal? (amb-take 1 (cons '(a) never)) '(a))
  (check-equal? (amb-take 2 (cons '(a) (λ () (λ () (cons '(b) never))))) '(a b))
  (check-equal? (amb-take 0 never) '())
  (check-exn #rx"tail forced" (λ () (amb-take 2 (cons '(a) never)))))

;; ---- two things a public interface would have to settle ----

(test-case "a stream is single-pass"
  ;; a pause or a rest is a thunk, not a promise: forcing it again runs
  ;; the rest of the computation again
  (define n 0)
  (define s (amb-reify (λ () (amb-pause) (set! n (add1 n)) n)))
  (check-equal? (list (amb-events (s)) (amb-events (s))) '((1) (2))))

(test-case "a parameter is read where the stream is forced"
  ;; The search is lazy, so a `parameterize` around the search, and not
  ;; in it, has ended when a later choice is made (amb as installed
  ;; behaves the same way).
  (define (later) (amb-pause) (amb (list 'l (count 0)) (list 'r (count 0))))
  (define s (with amb-interleave (amb-reify later)))
  (check-equal? (amb-events s 6)
                '(pause (l 0) pause (l 1) pause (l 2)))
  ;; in the search, the parameterization is part of the continuation
  (define t (amb-reify (λ () (with amb-interleave (later)))))
  (check-equal? (amb-events t 6)
                '(pause (l 0) pause (r 0) pause (l 1))))
