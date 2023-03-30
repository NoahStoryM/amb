#lang racket/base

(require "private/utils.rkt"
         (for-syntax racket/base syntax/parse)
         racket/contract
         data/queue)

(provide amb for/amb for*/amb
         (contract-out
          [current-amb-shuffler (parameter/c (-> list? list?))]
          [current-amb-queue    (parameter/c queue?)]
          [current-amb-enqueue! (parameter/c (-> queue? (-> none/c) void?))]
          [current-amb-dequeue! (parameter/c (-> queue? (-> none/c)))]
          [insert-amb-node*! (-> continuation? (listof (-> any)) void?)]))

(define-syntax amb
  (syntax-parser
    #:datum-literals (amb)
    [(_) #'(((current-amb-dequeue!) (current-amb-queue)))]
    [(_ alt0 ... (amb alt1 ...) alt2 ...)
     #'(amb alt0 ... alt1 ... alt2 ...)]
    [(_ alt ...+)
     #'(let/cc k
         (define alt* (list (λ () alt) ...))
         (insert-amb-node*! k alt*)
         (amb))]))

(define-syntaxes (for/amb for*/amb)
  (let ()
    (define-splicing-syntax-class break-clause
      [pattern (~seq (~or* #:break #:final) guard:expr)])
    (define (make-for/amb derived-stx)
      (syntax-parser
        [(_ (clause ...) break:break-clause ... body ...+)
         #`(let/cc k
             (define alt*
               (#,derived-stx (clause ...)
                #,@(apply append (syntax->list #'(break ...)))
                (λ () body ...)))
             (insert-amb-node*! k alt*)
             (amb))]))
    (values (make-for/amb #'for/list)
            (make-for/amb #'for*/list))))
