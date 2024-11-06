#lang racket/base

(require data/queue)
(provide (all-defined-out)
         (rename-out [call-with-no-continuation call/nc]))


(struct exn:fail:contract:amb exn:fail:contract ()
  #:extra-constructor-name make-exn:fail:contract:amb
  #:transparent)

(define call-with-no-continuation (λ (f) (f)))

(define current-amb-shuffler (make-parameter reverse))
(define current-amb-queue    (make-parameter (make-queue)))
(define current-amb-enqueue! (make-parameter enqueue-front!))
(define current-amb-dequeue! (make-parameter dequeue!))
(define current-amb-call     (make-parameter call-with-no-continuation))

(define schedule-amb-tasks!
  (λ (k alt*)
    (define amb-queue    (current-amb-queue))
    (define amb-enqueue! (current-amb-enqueue!))
    (for ([alt (in-list ((current-amb-shuffler) alt*))])
      (define (amb-task [k k]) (call-in-continuation k alt))
      (amb-enqueue! amb-queue amb-task))))
