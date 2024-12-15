#lang racket/base

(require racket/list racket/treelist racket/mutable-treelist)
(provide (all-defined-out))


(define (mutable-treelist-pop! mtl)
  (define pos (sub1 (mutable-treelist-length mtl)))
  (begin0 (mutable-treelist-ref mtl pos)
    (mutable-treelist-delete! mtl pos)))


(struct exn:fail:contract:amb exn:fail:contract ()
  #:extra-constructor-name make-exn:fail:contract:amb
  #:transparent)

(define (raise-amb-error)
  (raise (exn:fail:contract:amb
          "amb: empty amb tasks;\n expected at least one amb task\n  in: (amb)"
          (current-continuation-marks))))

(define current-amb-empty-handler (make-parameter raise-amb-error))
(define current-amb-shuffler (make-parameter reverse))
(define current-amb-tasks    (make-parameter (mutable-treelist)))
(define current-amb-pusher   (make-parameter mutable-treelist-add!))
(define current-amb-popper   (make-parameter mutable-treelist-pop!))

(define (schedule-amb-tasks! k alt* [amb-tasks (current-amb-tasks)])
  (define amb-push!   (current-amb-pusher))
  (define amb-shuffle (current-amb-shuffler))
  (if (or (eq? amb-push! mutable-treelist-add!)
          (eq? amb-push! mutable-treelist-cons!))
      (let ([alt*
             (cond
               [(eq? amb-push! mutable-treelist-add!) (amb-shuffle alt*)]
               [(eq? amb-shuffle reverse) alt*]
               [(eq? amb-shuffle shuffle) (shuffle alt*)]
               [else (amb-shuffle (reverse alt*))])])
        (define new-tasks
          (vector->treelist
           (for/vector #:length (length alt*) ([alt (in-list alt*)])
             (define (amb-task) (call-in-continuation k alt))
             amb-task)))
        (if (eq? amb-push! mutable-treelist-add!)
            (mutable-treelist-append!  amb-tasks new-tasks)
            (mutable-treelist-prepend! new-tasks amb-tasks)))
      (for ([alt (in-list (amb-shuffle alt*))])
        (define (amb-task) (call-in-continuation k alt))
        (amb-push! amb-tasks amb-task))))
