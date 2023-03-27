#lang racket/base

(require "private/utils.rkt"
         (for-syntax racket/base syntax/parse)
         racket/contract)

(provide amb for/amb for*/amb
         (contract-out
          [raise-amb-error (-> none/c)]
          [current-amb-shuffler (parameter/c (-> list? list?))]
          [current-amb-tree (parameter/c (-> none/c))]
          [make-amb-tree (->* (continuation? (listof (-> any)))
                              ((-> list? list?) (-> none/c))
                              (-> none/c))])
         (struct-out exn:fail:amb))

(define-syntax amb
  (syntax-parser
    #:datum-literals (amb)
    [(_) #'((current-amb-tree))]
    [(_ alt) #'alt]
    [(_ alt0 ... (amb alt1 ...) alt2 ...)
     #'(amb alt0 ... alt1 ... alt2 ...)]
    [(_ alt0 alt ...+)
     #'(let/cc k
         (define alt* (list (λ () alt) ...))
         (define amb-tree (make-amb-tree k alt*))
         (current-amb-tree amb-tree)
         alt0)]))

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
             (define amb-tree (make-amb-tree k alt*))
             (current-amb-tree amb-tree)
             (amb-tree))]))
    (values (make-for/amb #'for/list)
            (make-for/amb #'for*/list))))
