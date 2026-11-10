#lang racket/base

;; Local frontiers (`current-amb-local?`).
;; A choice point created with `current-amb-local?` #t schedules what is
;; created below it in a frontier of its own, where `(amb)` ends a turn.
(require rackunit racket/stream racket/list amb)

(define (run thk [n #f])
  (define s (in-amb (thk) #:empty-handler (λ () 'pause)))
  (if n (for/list ([v s] [_ n]) v) (stream->list s)))
(define-syntax-rule (local e) (parameterize ([current-amb-local? #t]) e))
;; a pause is a plain choice point, wherever it is called
(define (pause) (parameterize ([current-amb-local? #f]) (when (amb #t #f) (amb))))

(define (s1) (local (amb 1 2 3 (amb) 4 5 6)))
(define (s2) (local (amb 7 8 (amb) 9 (amb))))

(test-case "two streams of answers and pauses are merged by swapping at each pause"
  (check-equal? (run s1) '(1 2 3 pause 4 5 6))
  (check-equal? (run s2) '(7 8 pause 9 pause))
  (check-equal? (run (λ () (local (amb (s1) (s2)))))
                '(1 2 3 pause 7 8 pause 4 5 6 9 pause))
  ;; without a handler the pauses are invisible, the order is the same
  (check-equal? (stream->list (in-amb (local (amb (s1) (s2)))))
                '(1 2 3 7 8 4 5 6 9)))

(test-case "without current-amb-local? nothing changes"
  (define (s1) (amb 1 2 3 (amb) 4 5 6))
  (define (s2) (amb 7 8 (amb) 9 (amb)))
  (check-equal? (run (λ () (amb (s1) (s2))))
                '(1 2 3 pause 4 5 6 7 8 pause 9 pause)))

(test-case "plain choices in a local frontier take turns too"
  (define-syntax-rule (plain e) (parameterize ([current-amb-local? #f]) e))
  (define (s1) (plain (amb 1 2 3 (amb) 4 5 6)))
  (define (s2) (plain (amb 7 8 (amb) 9 (amb))))
  ;; s1 and s2 are plain trees in the frontier of the outer choice
  (check-equal? (run (λ () (local (amb (s1) (s2)))))
                '(1 2 3 pause 7 8 pause 4 5 6 9 pause)))

(test-case "the parameter covers the alternatives, like the other parameters"
  (define (s1) (amb 1 2 3 (amb) 4 5 6))
  (define (s2) (amb 7 8 (amb) 9 (amb)))
  (check-equal? (run (λ () (local (amb (s1) (s2)))))
                '(1 2 3 pause 7 8 pause 4 5 6 9 pause))
  (define (ticks name) (amb name (begin (when (amb #t #f) (amb)) (ticks name))))
  (check-equal? (for/list ([x (in-amb (local (amb (ticks 'a) (amb (ticks 'b) (ticks 'c)))))]
                           [_ 8])
                  x)
                '(a b a c a b a c))
  (check-equal? (for/list ([x (in-amb (local (amb (ticks 'a) (ticks 'b) (ticks 'c))))]
                           [_ 6])
                  x)
                '(a b c a b c)))

(test-case "amb-fail is not a turn"
  (define (s1) (local (amb 1 (amb-fail) 2)))
  (define (s2) (local (amb 3 4)))
  (check-equal? (run (λ () (local (amb (s1) (s2))))) '(1 2 3 4)))

(test-case "nested choices share turns as a binary tree: 1/2, 1/4, 1/4"
  (define ((ticker name)) (let loop () (pause) (amb name (loop))))
  (define l
    (run (λ () (local (amb ((ticker 'a)) (local (amb ((ticker 'b)) ((ticker 'c)))))))
         800))
  (check-equal? (for/list ([x '(a b c)]) (count (λ (v) (eq? v x)) l))
                '(200 100 99)))

(test-case "the order depends on the tree only: both groupings agree"
  (define (k v) (case v [(a) (pause) 'A] [(b) 'B] [(c) 'C]))
  (define (x) (local (amb 'a 'b)))
  (define (y) 'c)
  (check-equal? (run (λ () (k (local (amb (x) (y)))))) '(pause C B A))
  (check-equal? (run (λ () (local (amb (k (x)) (k (y)))))) '(pause C B A)))

(test-case "for/amb and make-amb take the parameter as amb does"
  (define expected '(1 2 3 pause 7 8 pause 4 5 6 9 pause))
  (check-equal? (run (λ () (local (for/amb ([s (list s1 s2)]) (s))))) expected)
  (check-equal? (run (λ () (local (for/amb #:length 2 ([s (list s1 s2)]) (s))))) expected)
  (check-equal? (run (λ () (let ([l (list s1 s2)])
                             ((local (make-amb (λ () (pair? l))
                                               (λ () (begin0 (car l) (set! l (cdr l))))))))))
                expected))

(test-case "the handler's values pass through the frontiers unchanged"
  (define (go handler)
    (for/list ([(a b) (in-amb (local (amb (local (amb (values 1 2) (amb) (values 3 4)))
                                          (values 5 6)))
                              #:empty-handler handler)])
      (list a b)))
  (check-equal? (go (λ () (values 'p 'q))) '((1 2) (p q) (5 6) (3 4)))
  (check-equal? (for/list ([(a b) (in-amb (local (amb (local (amb (values 1 2) (amb) (values 3 4)))
                                                      (values 5 6))))])
                  (list a b))
                '((1 2) (5 6) (3 4)))
  ;; a handler that returns no values still ends the turn
  (define out '())
  (for ([_ (in-amb/do (local (amb (local (amb (set! out (cons 1 out))
                                              (amb)
                                              (set! out (cons 3 out))))
                                  (set! out (cons 5 out))))
                      #:empty-handler (λ () (set! out (cons 'p out))))])
    (void))
  (check-equal? (reverse out) '(1 p 5 3)))

(test-case "a search inside a local choice has frontiers of its own"
  (check-equal?
   (run (λ () (local (amb (stream->list (in-amb (local (amb (s1) (s2)))
                                                #:empty-handler (λ () 'inner)))
                          (s2)))))
   '((1 2 3 inner 7 8 inner 4 5 6 9 inner) 7 8 pause 9 pause)))

(test-case "the stream of a search with local frontiers can be read twice"
  (define s (in-amb (local (amb (s1) (s2))) #:empty-handler (λ () 'pause)))
  (define s3 (stream-tail s 3))
  (check-equal? (stream->list s) '(1 2 3 pause 7 8 pause 4 5 6 9 pause))
  (check-equal? (stream->list s3) '(pause 7 8 pause 4 5 6 9 pause)))

(test-case "the other parameters keep their meaning inside a local frontier"
  ;; breadth-first below a local choice
  (check-equal? (run (λ () (local (parameterize ([current-amb-depth-first? #f])
                                    (amb 1 (amb 2 3) (amb 4 5 6) 7 8)))))
                '(1 7 8 2 3 4 5 6))
  ;; fair: a local choice is one item of the frontier around it, and
  ;; goes to the back of it after each leaf
  (define-syntax-rule (fair e) (parameterize ([current-amb-fair? #t]) e))
  (check-equal? (run (λ () (fair (amb (local (amb 1 2 3)) (local (amb 'a 'b 'c))))))
                '(1 a 2 b 3 c))
  (check-equal? (run (λ () (fair (amb (local (amb (amb 1 2) 3)) (local (amb 'a 'b 'c))))))
                '(1 a 3 b 2 c))
  (check-equal? (run (λ () (fair (amb (amb (amb 1 2) 3) (amb 'a 'b 'c)))))
                '(1 a 3 2 b c)))

(test-case "a long chain of finished choices does not slow the search down"
  (define (count-up n) (local (amb n (begin (pause) (count-up (add1 n))))))
  (check-equal? (length (run (λ () (count-up 0)) 20000)) 20000))
