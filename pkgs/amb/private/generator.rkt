#lang racket/base

;; ===================================================================
;; The generator protocol
;; ===================================================================
;;
;;   Generator = () -> (values Result Generator)
;;   Result    = list? | #f | exact-positive-integer?
;;
;;   a list   an answer: the values of one element; '() is an answer of
;;            no values
;;   #f       the end; the generator returned with it must not be called
;;   n        n pauses
;;
;; A list or #f is what `sequence-generate*` returns, so
;; (λ () (sequence-generate* seq)) is a generator.
;;
;; A generator is single-pass: it, and each generator it returns, is
;; called at most once.

(require (only-in "data/sequence.rkt" raise-empty-sequence-error))

(provide end pauses raise-result-error)

;; The generator of nothing.
(define (end) (values #f raise-empty-sequence-error))

;; n pauses, then g.
(define (pauses n g)
  (if (eq? n 0)
      g
      (λ () (values 1 (pauses (sub1 n) g)))))

;; A generator returned something that is not a Result.
(define (raise-result-error who v)
  (raise-argument-error who "(or/c list? exact-positive-integer? #f)" v))
