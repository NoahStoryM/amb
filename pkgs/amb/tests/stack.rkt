#lang racket/base

(require racket/set
         racket/stream
         rackunit)
(require "../main.rkt")

(displayln "Test `stack.rkt'")

(define (make-stack)
  (define stack (make-vector 1000010 #f))
  (vector-set! stack 1 1)
  stack)

(define (stack-length stack)
  (sub1 (vector-ref stack 1)))

(define (push! stack v)
  (define l (sub1 (vector-length stack)))
  (define i (vector-ref stack 1))
  (when (>= i l)
    (raise-range-error 'push! "stack" "ending " (add1 i) stack 1 l))
  (vector-set! stack i (vector-ref stack 0))
  (vector-set! stack 1 (add1 i))
  (vector-set! stack 0 v))

(define (pop! stack)
  (define l (sub1 (vector-length stack)))
  (define i (vector-ref stack 1))
    (when (<= i 1)
    (raise-range-error 'pop! "stack" "starting " (sub1 i) stack 1 l))
  (define v (vector-ref stack 0))
  (vector-set! stack i #f)
  (vector-set! stack 1 (sub1 i))
  (vector-set! stack 0 (if (= i 2) #f (vector-ref stack (sub1 i))))
  v)

(current-amb-maker  make-stack)
(current-amb-tasks  (make-stack))
(current-amb-length stack-length)
(current-amb-pusher push!)
(current-amb-popper pop!)


(test-case "Test efficiency"
  (parameterize ([current-amb-tasks ((current-amb-maker))])
    (time
     (define m 1000000)
     (define n (let next ([i 0]) (amb i (next (add1 i)))))
     (when (< n m) (amb))
     (check-eq? n m)))
  (parameterize ([current-amb-tasks ((current-amb-maker))])
    (define (next i j) (amb (values i j) (next (add1 i) (sub1 j))))
    (time
     (for ([i 1000000]
           [(j k) (in-amb* (λ () (next 0 0)))])
       (list i j k))))
  (parameterize ([current-amb-tasks ((current-amb-maker))])
    (define (next i j) (amb (values i j) (next (add1 i) (sub1 j))))
    (define s (in-amb* (λ () (next 0 0))))
    (time
     (for ([i 1000000]
           [(j k) (in-stream s)])
       (list i j k))))
  (parameterize ([current-amb-tasks ((current-amb-maker))])
    (define (next j) (amb j (next (add1 j))))
    (time
     (for ([i 1000000]
           [j (in-amb (next 0))])
       (list i j))))
  (parameterize ([current-amb-tasks ((current-amb-maker))])
    (define (next j) (amb j (next (add1 j))))
    (define s (in-amb (next 0)))
    (time
     (for ([i 1000000]
           [j (in-stream s)])
       (list i j)))))
