#lang racket/base

(require "private/utils.rkt"
         (for-syntax racket/base syntax/parse)
         racket/contract
         data/queue)

(provide amb amb* for/amb for*/amb
         (contract-out
          (struct exn:fail:contract:amb
            ([message string?]
             [continuation-marks continuation-mark-set?]))
          [current-amb-shuffler (parameter/c (-> list? list?))]
          [current-amb-queue    (parameter/c queue?)]
          [current-amb-enqueue! (parameter/c (-> queue? (-> none/c) void?))]
          [current-amb-dequeue! (parameter/c (-> queue? (-> none/c)))]
          [insert-amb-task*! (-> (-> any/c ... none/c) (listof (-> any)) void?)]))


(define-syntax amb
  (syntax-parser
    #:datum-literals (amb)
    [(_) #'(amb* (raise (exn:fail:contract:amb
                         "amb: empty amb queue;\n expected at least one amb task\n  in: (amb)"
                         (current-continuation-marks))))]
    [(_ alt0 ... (amb alt1 ...) alt2 ...)
     #'(amb alt0 ... alt1 ... alt2 ...)]
    [(_ alt ...+)
     #'(let/cc k
         (define alt* (list (λ () alt) ...))
         (insert-amb-task*! k alt*)
         (amb))]))

(define-syntax amb*
  (syntax-parser
    #:datum-literals ()
    [(_ v ...)
     #'(if (non-empty-queue? (current-amb-queue))
           (((current-amb-dequeue!)
             (current-amb-queue)))
           (values v ...))]))

(define-syntaxes (for/amb for*/amb)
  (let ()
    (define-splicing-syntax-class break-clause
      [pattern (~seq (~or* #:break #:final) guard:expr)])
    (define (make-for/amb derived-stx)
      (syntax-parser
        [(_ (clause ...) break:break-clause ... body ...+)
         #`(let/cc k
             (define alt* (#,derived-stx (clause ...) break ... (λ () body ...)))
             (insert-amb-task*! k alt*)
             (amb))]))
    (values (make-for/amb #'for/list)
            (make-for/amb #'for*/list))))
