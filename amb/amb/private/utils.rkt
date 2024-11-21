#lang racket/base

(provide (all-defined-out))


(struct exn:fail:contract:amb exn:fail:contract ()
  #:extra-constructor-name make-exn:fail:contract:amb
  #:transparent)

(define (raise-amb-error)
  (raise (exn:fail:contract:amb
          "amb: empty amb queue;\n expected at least one amb task\n  in: (amb)"
          (current-continuation-marks))))

(define current-amb-empty-handler (make-parameter raise-amb-error))
(define current-amb-shuffler      (make-parameter reverse))
(define current-amb-queue         (make-parameter (box '())))
(define current-amb-queue?        (make-parameter (λ (b) (and (box? b) (list?  (unbox b))))))
(define current-amb-queue-length  (make-parameter (λ (b) (and (box? b) (length (unbox b))))))
(define current-amb-queue-empty?  (make-parameter (λ (b) (and (box? b) (null?  (unbox b))))))
(define current-amb-make-queue    (make-parameter (λ () (box '()))))
(define current-amb-enqueue!      (make-parameter (λ (b v) (set-box! b (cons v (unbox b))))))
(define current-amb-dequeue!      (make-parameter (λ (b) (define ls (unbox b)) (set-box! b (cdr ls)) (car ls))))

(define (schedule-amb-tasks! alt* k)
  (define amb-queue    (current-amb-queue))
  (define amb-enqueue! (current-amb-enqueue!))
  (for ([alt (in-list ((current-amb-shuffler) alt*))])
    (define (amb-task) (call-in-continuation k alt))
    (amb-enqueue! amb-queue amb-task)))
