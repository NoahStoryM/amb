#lang typed/racket/base

(require (for-syntax racket/base syntax/parse)
         typed/racket/unsafe
         typed/data/queue)

(provide amb amb* for/amb for*/amb in-amb)

(unsafe-require/typed/provide amb/base
  [#:struct (exn:fail:contract:amb exn:fail:contract) ()]
  [amb* (∀ (a ...) (case→ (→ Null Nothing) (→ (Listof (→ (Values a ... a))) (Values a ... a))))]
  [in-amb* (∀ (a ...) (→ (→ (Values a ... a)) (Sequenceof a ... a)))]
  [raise-amb-error (→ Nothing)]
  [current-amb-empty-handler (Parameter (→ Nothing))]
  [current-amb-shuffler (Parameter (∀ (a) (→ (Listof a) (Listof a))))]
  [current-amb-queue    (Parameter (Queue (→ Nothing) (→ Nothing)))]
  [current-amb-enqueue! (Parameter (→ (Queue (→ Nothing) Any) (→ Nothing) Void))]
  [current-amb-dequeue! (Parameter (→ (Queue Nothing (→ Nothing)) (→ Nothing)))]
  [schedule-amb-tasks!  (∀ (a ...) (→* ((→ a ... a Nothing) (Listof (→ (Values a ... a)))) ((Queue (→ Nothing) Any)) Void))])


(define-syntax (amb stx)
  (syntax-parse stx
    #:datum-literals ()
    [(_ expr ...)
     (syntax/loc stx (amb* (list (λ () expr) ...)))]))


(define-syntaxes (for/amb for*/amb)
  (let ()
    (define-splicing-syntax-class break-clause
      [pattern (~seq (~or* #:break #:final) guard:expr)])
    (define (alt*-parser derived-stx)
      (define parser
        (syntax-parser
          #:datum-literals (:)
          [(_ : t1 (clause ...) : t2 break:break-clause ... body ...+)
           #`(#,derived-stx
              : (Listof (→ t1))
              (clause ...)
              : (Listof (→ t2))
              break ...
              (ann (ann (λ () body ...) (→ t2)) (→ t1)))]
          [(~or* (name : t0 (clause ...) break:break-clause ... body ...+)
                 (name (clause ...) : t0 break:break-clause ... body ...+)
                 (name (clause ...) break:break-clause ... body ...+))
           #:with t (if (attribute t0) #'t0 #'AnyValues)
           (parser #'(name : t (clause ...) : t break ... body ...))]))
      parser)
    (define ((make-for/amb derived-stx) stx)
      (quasisyntax/loc stx
        (amb* #,((alt*-parser derived-stx) stx))))
    (values (make-for/amb #'for/list)
            (make-for/amb #'for*/list))))


(define-syntax (in-amb stx)
  (syntax-parse stx
    #:datum-literals ()
    [(_ expr)
     (syntax/loc stx (in-amb* (λ () expr)))]))