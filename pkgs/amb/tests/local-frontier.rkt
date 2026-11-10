#lang racket/base

;; RESEARCH: local frontiers (`current-amb-local?`).
;; A choice point created with `current-amb-local?` #t schedules what is
;; created below it in a frontier of its own; `(amb)` is a yield.
(require rackunit racket/stream racket/list amb)

(define (run thk [n #f])
  (define s (in-amb (thk) #:empty-handler (λ () 'pause)))
  (if n (for/list ([v s] [_ n]) v) (stream->list s)))
(define-syntax-rule (local e) (parameterize ([current-amb-local? #t]) e))
;; a pause is a plain choice point, wherever it is called
(define (pause) (parameterize ([current-amb-local? #f]) (amb (amb) (void))))

(test-case "two streams of answers and pauses are merged by swapping at each pause"
  (define (s1) (local (amb 1 2 3 (amb) 4 5 6)))
  (define (s2) (local (amb 7 8 (amb) 9 (amb))))
  (check-equal? (run s1) '(1 2 3 pause 4 5 6))
  (check-equal? (run s2) '(7 8 pause 9 pause))
  (check-equal? (run (λ () (local (amb (s1) (s2)))))
                '(1 2 3 pause 7 8 pause 4 5 6 9 pause))
  ;; without a handler the pauses are invisible, the order is the same
  (check-equal? (stream->list (in-amb (local (amb (s1) (s2)))))
                '(1 2 3 7 8 4 5 6 9)))

(test-case "nested choices share turns as a binary tree: 1/2, 1/4, 1/4"
  (define ((ticker name)) (let loop () (amb (amb) (amb name (loop)))))
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

(test-case "without current-amb-local? nothing changes"
  (define (s1) (amb 1 2 3 (amb) 4 5 6))
  (define (s2) (amb 7 8 (amb) 9 (amb)))
  (check-equal? (run (λ () (amb (s1) (s2))))
                '(1 2 3 pause 4 5 6 7 8 pause 9 pause)))

(test-case "a long chain of finished choices does not slow the search down"
  (define (count-up n) (local (amb n (begin (pause) (count-up (add1 n))))))
  (check-equal? (length (run (λ () (count-up 0)) 20000)) 20000))
