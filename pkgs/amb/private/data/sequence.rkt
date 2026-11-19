#lang racket/base

(require "../base.rkt"
         racket/sequence)
(provide empty-sequence
         initiate-sequence
         raise-empty-sequence-error
         make-sequence)

;; With `advance`, the sequence holds the raw generator.  `advance` takes
;; the generator and returns the same two values as a call to `next0`
;; would.  It receives #f on reentry or after an interrupted read.
(define (generate*->generate next0 [advance #f])
  (define v0* #f)
  (define done? #f)
  ;; Once exhausted, `more?` keeps returning #f without calling `next0`
  ;; again, as other single-pass sequences (such as `in-port`) do.  This
  ;; matters when several generators share the same underlying sequence.
  (define (more? . _)
    (or (and v0* #t)
        (and (not done?)
             (let-values ([(v1* next1)
                           (if advance
                               (advance (begin0 next0 (set! next0 #f)))
                               (next0))])
               (set! v0* v1*)
               (set! next0 next1)
               (set! done? (not v1*))
               (and v1* #t)))))
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

(define (make-sequence next [advance #f])
  (define-values (more? get) (generate*->generate next advance))
  (make-do-sequence
   (λ ()
     (initiate-sequence
      #:init-pos           0
      #:continue-with-pos? more?
      #:pos->element       get
      #:next-pos           add1))))
