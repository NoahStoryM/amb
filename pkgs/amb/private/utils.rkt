#lang racket/base

(require data/queue)
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


(struct exn:fail:contract:amb exn:fail:contract ()
  #:extra-constructor-name make-exn:fail:contract:amb
  #:transparent)

(define (raise-amb-error)
  (raise (exn:fail:contract:amb
          "amb: empty amb tasks;\n expected at least one amb task\n  in: (amb)"
          (current-continuation-marks))))

(define current-amb-empty-handler (make-parameter raise-amb-error))
(define current-amb-shuffler (make-parameter vector-reverse!))
(define current-amb-maker    (make-parameter make-queue))
(define current-amb-tasks    (make-parameter (make-queue)))
(define current-amb-length   (make-parameter queue-length))
(define current-amb-pusher   (make-parameter enqueue-front!))
(define current-amb-popper   (make-parameter dequeue!))
