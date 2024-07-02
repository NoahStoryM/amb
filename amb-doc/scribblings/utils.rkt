#lang racket/base

(require scribble/manual
         scribble/example
         (for-syntax racket/base
                     syntax/parse))

(provide (except-out (all-defined-out) main))

(define (make-amb-eval) (make-base-eval #:lang 'racket/base '(require data/queue amb)))
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
                alt)
         (apply tech
                #:key          key
                #:normalize?   normalize?
                #:doc          module-path
                #:tag-prefixes prefixes
                #:indirect?    indirect?
                alt)))]))

(define-tech tech/guide '(lib "scribblings/guide/guide.scrbl"))

(define main (λ ([argv (current-command-line-arguments)]) (values)))
(module+ main (call-with-values main exit))
