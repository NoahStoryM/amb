#lang racket/base

(require data/queue)
(provide (all-defined-out))


(struct exn:fail:contract:amb exn:fail:contract ()
  #:extra-constructor-name make-exn:fail:contract:amb
  #:transparent)

(define (raise-amb-error)
  (raise (exn:fail:contract:amb
          "amb: empty amb queue;\n expected at least one amb task\n  in: (amb)"
          (current-continuation-marks))))

(define current-amb-empty-handler (make-parameter raise-amb-error))
(define current-amb-shuffler (make-parameter reverse))
(define current-amb-queue    (make-parameter (make-queue)))
(define current-amb-enqueue! (make-parameter enqueue-front!))
(define current-amb-dequeue! (make-parameter dequeue!))

(define schedule-amb-tasks!
  (λ (alt* k)
    (unless (null? alt*)
      (define amb-queue    (current-amb-queue))
      (define amb-enqueue! (current-amb-enqueue!))
      (for ([alt (in-list ((current-amb-shuffler) alt*))])
        (define (amb-task) (call-in-continuation k alt))
        (amb-enqueue! amb-queue amb-task)))))
