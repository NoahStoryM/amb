#lang racket/base

(require "../base.rkt"
         racket/sequence)
(provide empty-sequence
         initiate-sequence
         generate*->generate
         raise-empty-sequence-error
         make-sequence)

(define (generate*->generate next0)
  (define v0* #f)
  (define (more? . _)
    (or (and v0* #t)
        (let-values ([(v1* next1) (next0)])
          (set! v0* v1*)
          (set! next0 next1)
          (and v0* #t))))
  (define (get . _)
    (if (more?)
        (let ([v1* v0*])
          (set! v0* #f)
          (apply values v1*))
        (raise-empty-sequence-error)))
  (values more? get))

(define (raise-empty-sequence-error)
  (raise (exn:fail:contract
          "sequence has no more values"
          (current-continuation-marks))))

(define (make-sequence next)
  (define-values (more? get) (generate*->generate next))
  (make-do-sequence
   (λ ()
     (initiate-sequence
      #:init-pos           0
      #:continue-with-pos? more?
      #:pos->element       get
      #:next-pos           add1))))
