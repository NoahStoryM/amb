#lang racket/base

(provide any? none?)

(define (any?  . _) #t)
(define (none? . _) #f)
