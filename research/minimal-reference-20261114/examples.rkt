#lang racket/base
(require racket/sequence "kernel.rkt")

;; All observations below are finite, including the two infinite sources.
(displayln (for/list ([x (in-amb (+ 10 (amb 1 2 3)))]) x))
(displayln (for/list ([x (in-amb (for/amb ([i 5]) (* i i)))]) x))
(displayln (for/list ([(x y) (in-amb (amb (values 1 2) (values 3 4)))]) (+ x y)))
(displayln (for/list ([vs (sequence-map list (in-amb (amb (values) (values #f 'pause))))]) vs))
(displayln (for/list ([_ (in-range 5)]
                     [x (in-amb (make-amb (λ () (sequence-generate* (in-naturals)))))]) x))
(displayln (for/list ([x (in-amb (begin (amb-pause) 42) #:on-pause (λ () 'paused))]) x))

(define (count n)
  (amb n (begin (amb-pause) (count (add1 n)))))

(for ([name '(append interleave alternate level)]
      [merge (list amb-append amb-interleave amb-alternate amb-level)])
  (parameterize ([current-amb-merge merge])
    (displayln
     (cons name
           (for/list ([_ (in-range 9)]
                      [event (in-amb (amb (list 'left (count 0))
                                          (list 'right (count 0)))
                                     #:on-pause (λ () 'pause))])
             event)))))
