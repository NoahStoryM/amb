#lang racket/base

(require racket/promise (for-syntax racket/base syntax/parse))

(provide amb for/amb for*/amb
         (struct-out exn:fail:amb)
         raise-amb-error)

(struct exn:fail:amb exn:fail ()
  #:extra-constructor-name make-exn:fail:amb
  #:transparent)


(define raise-amb-error
  (λ ()
    (raise
     (make-exn:fail:amb
      "amb tree exhausted"
      (current-continuation-marks)))))

(define continue raise-amb-error)

(define update-continue!
  (λ (k alt*)
    (define prev-continue continue)
    (set! continue
          (λ ()
            (if (null? alt*)
                (begin
                  (set! continue prev-continue)
                  (prev-continue))
                (let ([alt1 (force (car alt*))])
                  (set! alt* (cdr alt*))
                  (k alt1)))))))

(define-syntax amb
  (syntax-parser
    [(_) #'(continue)]
    [(_ alt) #'alt]
    [(_ alt0 ... (amb alt1 ...) alt2 ...)
     #'(amb alt0 ... alt1 ... alt2 ...)]
    [(_ alt0 alt ...+)
     #'(let/cc k
         (define alt* (list (delay alt) ...))
         (update-continue! k alt*)
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
                (delay body ...)))
             (update-continue! k alt*)
             (continue))]))
    (values (make-for/amb #'for/list)
            (make-for/amb #'for*/list))))
