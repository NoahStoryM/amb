#lang racket/base

;; Local frontiers (`amb/local`).
;; A choice made with `amb/local` schedules what is created below it in a
;; frontier of its own, where `(amb)` ends a turn.
(require rackunit racket/stream racket/list amb)

(define (run thk [n #f])
  (define s (in-amb (thk) #:empty-handler (λ () 'pause)))
  (if n (for/list ([v s] [_ n]) v) (stream->list s)))
;; a pause is a plain choice point, in whatever frontier is current
(define (pause) (when (amb #t #f) (amb)))

(define (s1) (amb/local 1 2 3 (amb) 4 5 6))
(define (s2) (amb/local 7 8 (amb) 9 (amb)))

(test-case "two streams of answers and pauses are merged by swapping at each pause"
  (check-equal? (run s1) '(1 2 3 pause 4 5 6))
  (check-equal? (run s2) '(7 8 pause 9 pause))
  (check-equal? (run (λ () (amb/local (s1) (s2))))
                '(1 2 3 pause 7 8 pause 4 5 6 9 pause))
  ;; without a handler the pauses are invisible, the order is the same
  (check-equal? (stream->list (in-amb (amb/local (s1) (s2))))
                '(1 2 3 7 8 4 5 6 9)))

(test-case "without amb/local nothing changes"
  (define (s1) (amb 1 2 3 (amb) 4 5 6))
  (define (s2) (amb 7 8 (amb) 9 (amb)))
  (check-equal? (run (λ () (amb (s1) (s2))))
                '(1 2 3 pause 4 5 6 7 8 pause 9 pause)))

(test-case "plain choices in a local frontier take turns too"
  (define (s1) (amb 1 2 3 (amb) 4 5 6))
  (define (s2) (amb 7 8 (amb) 9 (amb)))
  ;; s1 and s2 are plain trees in the frontier of the outer choice
  (check-equal? (run (λ () (amb/local (s1) (s2))))
                '(1 2 3 pause 7 8 pause 4 5 6 9 pause)))

(test-case "the alternatives are born in the local frontier, a nested amb/local makes another"
  (define (ticks name) (amb name (begin (pause) (ticks name))))
  ;; a plain inner choice: its alternatives are items of the same frontier
  (check-equal? (for/list ([x (in-amb (amb/local (ticks 'a) (amb (ticks 'b) (ticks 'c))))]
                           [_ 6])
                  x)
                '(a b c a b c))
  ;; a local inner choice is one item of the outer frontier
  (check-equal? (for/list ([x (in-amb (amb/local (ticks 'a) (amb/local (ticks 'b) (ticks 'c))))]
                           [_ 8])
                  x)
                '(a b a c a b a c))
  (check-equal? (for/list ([x (in-amb (amb/local (ticks 'a) (ticks 'b) (ticks 'c)))]
                           [_ 6])
                  x)
                '(a b c a b c)))

(test-case "amb-fail is not a turn"
  (define (s1) (amb/local 1 (amb-fail) 2))
  (define (s2) (amb/local 3 4))
  (check-equal? (run (λ () (amb/local (s1) (s2)))) '(1 2 3 4)))

(test-case "nested choices share turns as a binary tree: 1/2, 1/4, 1/4"
  (define ((ticker name)) (let loop () (pause) (amb name (loop))))
  (define l
    (run (λ () (amb/local ((ticker 'a)) (amb/local ((ticker 'b)) ((ticker 'c)))))
         800))
  (check-equal? (for/list ([x '(a b c)]) (count (λ (v) (eq? v x)) l))
                '(200 100 99)))

(test-case "the order depends on the tree only: both groupings agree"
  (define (k v) (case v [(a) (pause) 'A] [(b) 'B] [(c) 'C]))
  (define (x) (amb/local 'a 'b))
  (define (y) 'c)
  (check-equal? (run (λ () (k (amb/local (x) (y))))) '(pause C B A))
  (check-equal? (run (λ () (amb/local (k (x)) (k (y))))) '(pause C B A)))

(test-case "the handler's values pass through the frontiers unchanged"
  (define (go handler)
    (for/list ([(a b) (in-amb (amb/local (amb/local (values 1 2) (amb) (values 3 4))
                                         (values 5 6))
                              #:empty-handler handler)])
      (list a b)))
  (check-equal? (go (λ () (values 'p 'q))) '((1 2) (p q) (5 6) (3 4)))
  (check-equal? (for/list ([(a b) (in-amb (amb/local (amb/local (values 1 2) (amb) (values 3 4))
                                                     (values 5 6)))])
                  (list a b))
                '((1 2) (5 6) (3 4)))
  ;; a handler that returns no values still ends the turn
  (define out '())
  (for ([_ (in-amb/do (amb/local (amb/local (set! out (cons 1 out))
                                            (amb)
                                            (set! out (cons 3 out)))
                                 (set! out (cons 5 out)))
                      #:empty-handler (λ () (set! out (cons 'p out))))])
    (void))
  (check-equal? (reverse out) '(1 p 5 3)))

(test-case "a search inside a local choice has frontiers of its own"
  (check-equal?
   (run (λ () (amb/local (stream->list (in-amb (amb/local (s1) (s2))
                                               #:empty-handler (λ () 'inner)))
                         (s2))))
   '((1 2 3 inner 7 8 inner 4 5 6 9 inner) 7 8 pause 9 pause)))

(test-case "the stream of a search with local frontiers can be read twice"
  (define s (in-amb (amb/local (s1) (s2)) #:empty-handler (λ () 'pause)))
  (define s3 (stream-tail s 3))
  (check-equal? (stream->list s) '(1 2 3 pause 7 8 pause 4 5 6 9 pause))
  (check-equal? (stream->list s3) '(pause 7 8 pause 4 5 6 9 pause)))

(test-case "the other parameters keep their meaning inside a local frontier"
  ;; breadth-first below a local choice
  (check-equal? (run (λ () (parameterize ([current-amb-depth-first? #f])
                             (amb/local 1 (amb 2 3) (amb 4 5 6) 7 8))))
                '(1 7 8 2 3 4 5 6))
  ;; fair: a local choice is one item of the frontier around it, and
  ;; goes to the back of it after each leaf
  (define-syntax-rule (fair e) (parameterize ([current-amb-fair? #t]) e))
  (check-equal? (run (λ () (fair (amb (amb/local 1 2 3) (amb/local 'a 'b 'c)))))
                '(1 a 2 b 3 c))
  (check-equal? (run (λ () (fair (amb (amb/local (amb 1 2) 3) (amb/local 'a 'b 'c)))))
                '(1 a 3 b 2 c))
  (check-equal? (run (λ () (fair (amb (amb (amb 1 2) 3) (amb 'a 'b 'c)))))
                '(1 a 3 2 b c)))

(test-case "every strategy: local frontiers keep the answers and the pauses"
  ;; a small random-ish tree of local and plain choices with pauses
  (define (tree d i)
    (cond
      [(zero? d) (when (odd? i) (pause)) i]
      [(even? (+ d i)) (amb/local (tree (sub1 d) (* 2 i)) (tree (sub1 d) (add1 (* 2 i))))]
      [else (amb (tree (sub1 d) (* 2 i)) (begin (pause) (tree (sub1 d) (add1 (* 2 i)))))]))
  (define (plain d i)
    (cond
      [(zero? d) (when (odd? i) (pause)) i]
      [else (amb (plain (sub1 d) (* 2 i)) (begin (when (odd? (+ d i)) (pause))
                                                  (plain (sub1 d) (add1 (* 2 i)))))]))
  (define expected
    (sort (filter number? (run (λ () (plain 5 1)))) <))
  (for* ([df '(#t #f)] [fair '(#t #f)])
    (define r (parameterize ([current-amb-depth-first? df] [current-amb-fair? fair])
                (run (λ () (tree 5 1)))))
    (check-equal? (sort (filter number? r) <) expected (format "df=~a fair=~a" df fair))
    (check-equal? (count (λ (v) (eq? v 'pause)) r)
                  (count (λ (v) (eq? v 'pause)) (run (λ () (plain 5 1))))
                  (format "pauses: df=~a fair=~a" df fair))))

(test-case "every strategy: the two groupings agree"
  (define (k v) (case v [(a) (pause) 'A] [(b) 'B] [(c) (pause) 'C] [(d) 'D]))
  (define (x) (amb/local 'a 'b))
  (define (y) (amb/local 'c 'd))
  (for* ([df '(#t #f)] [fair '(#t #f)])
    (parameterize ([current-amb-depth-first? df] [current-amb-fair? fair])
      (check-equal? (run (λ () (k (amb/local (x) (y)))))
                    (run (λ () (amb/local (k (x)) (k (y)))))
                    (format "df=~a fair=~a" df fair)))))
