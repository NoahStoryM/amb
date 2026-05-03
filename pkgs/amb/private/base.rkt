#lang racket/base

(require (for-syntax racket/base syntax/parse))
(provide let*-values)


(define-syntax (let*-values stx)
  (syntax-parse stx
    [(_ () body ...+) (syntax/loc stx (let () body ...))]
    [(_ ([formals expr]) body ...+)
     (syntax/loc stx
       (call-with-values
        (λ () expr)
        (λ formals body ...)))]
    [(_ ([formals expr] [formals* expr*] ...) body ...+)
     (syntax/loc stx
       (let*-values ([formals expr])
         (let*-values ([formals* expr*] ...)
           body ...)))]))
