#lang typed/racket/base

(require (for-syntax racket/base syntax/parse)
         typed/racket/mutable-treelist/lab
         typed/racket/unsafe)

(provide amb amb* amb*₁ for/amb for*/amb in-amb in-amb₁)

(unsafe-require/typed/provide amb/core
  [#:struct (exn:fail:contract:amb exn:fail:contract) ()]
  [amb*  (∀ (a ...) (case→ (→                  Nothing) (→                   (→ (Values a ... a)) * (Values a ... a))))]
  [amb*₁ (∀ (a ...) (case→ (→ (Mutable-Vector) Nothing) (→ (Mutable-Vectorof (→ (Values a ... a)))  (Values a ... a))))]
  [in-amb*  (∀ (a ...) (→ (→ (Values a ... a)) (Sequenceof a ... a)))]
  [in-amb*₁ (∀ (a ...) (→ (→ (Values a ... a)) (Sequenceof a ... a)))]
  [raise-amb-error (→ Nothing)]
  [current-amb-empty-handler (Parameter (→ Nothing))]
  [current-amb-shuffler (Parameter (∀ (a) (→ (Mutable-Vectorof a) Void)))]
  [current-amb-tasks    (Parameter (Mutable-TreeListof (→ Nothing) (→ Nothing)))]
  [current-amb-pusher   (Parameter (→ (Mutable-TreeListof (→ Nothing) Any) (→ Nothing) Void))]
  [current-amb-popper   (Parameter (→ (Mutable-TreeListof Nothing (→ Nothing)) (→ Nothing)))])


(define-syntax (amb stx)
  (syntax-parse stx
    #:datum-literals ()
    [(_ expr ...)
     (syntax/loc stx (amb* (λ () expr) ...))]))


(define-syntaxes (for/amb for*/amb)
  (let ()
    (define-splicing-syntax-class length-clause
      [pattern (~seq #:length n:expr (~optional (~seq #:fill fill:expr)))])
    (define-splicing-syntax-class break-clause
      [pattern (~seq (~or* #:break #:final) guard:expr)])
    (define (make-for/amb derived-stx)
      (define (parser stx)
        (syntax-parse stx
          #:datum-literals (:)
          [(_ : t1 #:length n #:fill fill (clauses ...) : t2 break:break-clause ... body ...+)
           (quasisyntax/loc stx
             (amb*₁
              (#,derived-stx
               : (Mutable-Vectorof (→ t1))
               #:length n
               #:fill (ann (λ () : t2 fill) (→ t1))
               (clauses ...)
               : (→ t2)
               break ...
               (ann (λ () : t2 body ...) (→ t1)))))]
          [(name : t1 #:length n (clauses ...) : t2 break:break-clause ... body ...+)
           (parser (syntax/loc stx (name : t1 #:length n #:fill 0 (clauses ...) : t2 break ... body ...)) )]
          [(name : t1 (clauses ...) : t2 break:break-clause ... body ...+)
           (quasisyntax/loc stx
             (amb*₁
              (#,derived-stx
               : (Mutable-Vectorof (→ t1))
               (clauses ...)
               : (→ t2)
               break ...
               (ann (λ () : t2 body ...) (→ t1)))))]
          [(~or* (name : t0 (~optional length:length-clause) (clauses ...) break:break-clause ... body ...+)
                 (name (~optional length:length-clause) (clauses ...) : t0 break:break-clause ... body ...+)
                 (name (~optional length:length-clause) (clauses ...) break:break-clause ... body ...+))
           #:with t (if (attribute t0) #'t0 #'AnyValues)
           #:with (maybe-length ...) (if (attribute length) #'length #'())
           (parser (syntax/loc stx (name : t maybe-length ... (clauses ...) : t break ... body ...)))]))
      parser)
    (values (make-for/amb #'for/vector)
            (make-for/amb #'for*/vector))))


(define-syntaxes (in-amb in-amb₁)
  (let ()
    (define ((make derived-stx) stx)
      (syntax-parse stx
        #:datum-literals ()
        [(_ expr)
         (quasisyntax/loc stx
           (#,derived-stx (λ () expr)))]))
    (values (make #'in-amb*)
            (make #'in-amb*₁))))