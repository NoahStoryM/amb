#lang racket/base

(provide (all-defined-out))

(struct exn:fail:amb exn:fail ()
  #:extra-constructor-name make-exn:fail:amb
  #:transparent)


(define raise-amb-error
  (λ ()
    (raise
     (make-exn:fail:amb
      "amb tree exhausted"
      (current-continuation-marks)))))

(define current-amb-shuffler (make-parameter values))
(define current-amb-tree (make-parameter raise-amb-error))

(define make-amb-tree
  (λ (k alt* [amb-shuffler (current-amb-shuffler)] [previous-amb-tree (current-amb-tree)])
    (let ([alt* (amb-shuffler alt*)])
      (define amb-tree
        (λ ()
          (if (null? alt*)
              (previous-amb-tree)
              (let ([alt0 (car alt*)])
                (set! alt* (cdr alt*))
                (call-with-values alt0 k)))))
      amb-tree)))
