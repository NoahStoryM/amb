#lang racket/base

(require data/queue)
(provide (all-defined-out))


(define current-amb-shuffler (make-parameter reverse))
(define current-amb-queue    (make-parameter (make-queue)))
(define current-amb-enqueue! (make-parameter enqueue-front!))
(define current-amb-dequeue! (make-parameter dequeue!))

(define insert-amb-node*!
  (λ (k alt*)
    (define amb-queue    (current-amb-queue))
    (define amb-enqueue! (current-amb-enqueue!))
    (for ([alt (in-list ((current-amb-shuffler) alt*))])
      (define amb-node (λ () (call-with-values alt k)))
      (amb-enqueue! amb-queue amb-node))))
