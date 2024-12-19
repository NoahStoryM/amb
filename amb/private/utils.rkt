#lang racket/base

(require racket/mutable-treelist)
(provide (all-defined-out))


(define (vector-reverse! v)
  (define len (vector-length v))
  (when (<= 2 len)
    (for ([i (in-range 0 len)]
          [j (in-range (sub1 len) -1 -1)])
      #:final (>= 2 (- j i))
      (define vi (vector-ref v i))
      (define vj (vector-ref v j))
      (vector-set! v i vj)
      (vector-set! v j vi))))

(define (mutable-treelist-pop! mtl)
  (begin0 (mutable-treelist-last mtl)
    (mutable-treelist-delete! mtl (sub1 (mutable-treelist-length mtl)))))


(struct exn:fail:contract:amb exn:fail:contract ()
  #:extra-constructor-name make-exn:fail:contract:amb
  #:transparent)

(define (raise-amb-error)
  (raise (exn:fail:contract:amb
          "amb: empty amb tasks;\n expected at least one amb task\n  in: (amb)"
          (current-continuation-marks))))

(define current-amb-empty-handler (make-parameter raise-amb-error))
(define current-amb-maker    (make-parameter mutable-treelist))
(define current-amb-tasks    (make-parameter (mutable-treelist)))
(define current-amb-shuffler (make-parameter vector-reverse!))
(define current-amb-length   (make-parameter mutable-treelist-length))
(define current-amb-pusher   (make-parameter mutable-treelist-add!))
(define current-amb-popper   (make-parameter mutable-treelist-pop!))
