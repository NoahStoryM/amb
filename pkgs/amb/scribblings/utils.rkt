#lang racket/base

(require scribble/manual
         scribble/example
         (for-syntax racket/base
                     syntax/parse))

(provide (all-defined-out))

(define (make-amb-eval) (make-base-eval #:lang 'racket/base '(require amb data/queue)))
(define-syntax-rule (amb-examples body ...) (examples #:eval (make-amb-eval) body ...))

(define-syntax (define-tech stx)
  (syntax-parse stx
    [(_ tech/name module-path)
     (syntax/loc stx
       (define (tech/name
                #:key          [key        #f]
                #:normalize?   [normalize? #t]
                #:tag-prefixes [prefixes   #f]
                #:indirect?    [indirect?  #f]
                .
                pre-content*)
         (apply tech
                #:key          key
                #:normalize?   normalize?
                #:doc          module-path
                #:tag-prefixes prefixes
                #:indirect?    indirect?
                pre-content*)))]))

(define-tech tech/guide '(lib "scribblings/guide/guide.scrbl"))
(define-tech tech/refer '(lib "scribblings/reference/reference.scrbl"))
