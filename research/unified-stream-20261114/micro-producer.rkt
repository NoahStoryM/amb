#lang racket/base
(provide pair-node values-node closure-node thunk-node)
;; Loaded through dynamic-require with a runtime symbol: caller cannot simply
;; inline the producer and cancel its constructor against the consumer.
(define (pair-node x tail) (cons x tail))
(define (values-node x tail) (values x tail))
(define (closure-node x tail) (λ () (values x tail)))
(define (thunk-node x tail) (λ () x))
