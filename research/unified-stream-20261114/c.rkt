#lang racket/base
(require (for-syntax racket/base syntax/parse)
         racket/control racket/stream racket/include "catenable.rkt")
(provide (all-defined-out))
(include "values-core.inc")
(include "choice-macros.inc")

;; C produces one pause at a time; counts are an input construction helper.
(define (pause-stream n next)
  (unless (exact-positive-integer? n)
    (raise-argument-error 'pause-stream "exact-positive-integer?" n))
  (λ () (values 1 (if (= n 1) next (pause-stream (sub1 n) next)))))
(define (after-pause r next)
  (unless (eqv? r 1) (error 'C "expected pause 1, got ~e" r))
  next)
(define (finish-round ws)
  (values 1 (leveled '() (cat-first ws) (cat-rest ws))))
