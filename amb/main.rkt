#lang racket/base

(require (for-syntax racket/base syntax/parse)
         racket/contract)

(provide amb for/amb for*/amb
         (contract-out
          [raise-amb-error (-> none/c)]
          [make-amb-tree (->* (continuation? (listof (-> any))) ((-> none/c)) (-> none/c))]
          [current-amb-tree (parameter/c (-> none/c))])
         (struct-out exn:fail:amb))

(struct exn:fail:amb exn:fail ()
  #:extra-constructor-name make-exn:fail:amb
  #:transparent)


(define raise-amb-error
  (λ ()
    (raise
     (make-exn:fail:amb
      "amb tree exhausted"
      (current-continuation-marks)))))

(define current-amb-tree (make-parameter raise-amb-error))

(define make-amb-tree
  (λ (k alt* [previous-amb-tree (current-amb-tree)])
    (define amb-tree
      (λ ()
        (if (null? alt*)
            (previous-amb-tree)
            (let ([alt1 (car alt*)])
              (set! alt* (cdr alt*))
              (call-with-values alt1 k)))))
    amb-tree))

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
