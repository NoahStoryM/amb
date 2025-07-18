#lang typed/racket/base/shallow

;; Typed wrapper around the base `amb` implementation.  All functions
;; are re-exported with precise types for use in Typed Racket
;; programs.

(require (for-syntax racket/base syntax/parse)
         typed/goto)

(provide amb amb* unsafe-amb* for/amb for*/amb in-amb in-amb/do)

(require/typed/provide amb/private/amb
  [amb*        (∀ (a ...) (case→ (→                  Nothing) (→                   (→ (Values a ... a)) * (Values a ... a))))]
  [unsafe-amb* (∀ (a ...) (case→ (→ (Mutable-Vector) Nothing) (→ (Mutable-Vectorof (→ (Values a ... a)))  (Values a ... a))))]
  [in-amb*    (∀ (a ...) (→ (→ (Values a ... a)) (Sequenceof a ... a)))]
  [in-amb*/do (∀ (a ...) (→ (→ (Values a ... a)) (Sequenceof a ... a)))]
  [#:struct (exn:fail:contract:amb exn:fail:contract) ()]
  [raise-amb-error (→ Nothing)]
  [current-amb-empty-handler (Parameter (→ Nothing))]
  [current-amb-shuffler (Parameter (→ Mutable-VectorTop Void))]
  [current-amb-maker    (Parameter (→ (Sequenceof Label)))]
  [current-amb-tasks    (Parameter (Sequenceof Label))]
  [current-amb-length   (Parameter (→ SequenceTop Index))]
  [current-amb-pusher   (Parameter (∀ (a) (→ (Sequenceof a) a Void)))]
  [current-amb-popper   (Parameter (∀ (a) (→ (Sequenceof a) a)))])


(define-syntax (amb stx)
  ;; Typed version of the `amb` macro.  Each expression is delayed and
  ;; processed by the underlying implementation.
  (syntax-parse stx
    #:datum-literals ()
    [(_ expr ...)
     (syntax/loc stx
       (amb* (λ () expr) ...))]))


(define-syntaxes (for/amb for*/amb)
  ;; Typed versions of the `for/amb` and `for*/amb` macros.  They reuse
  ;; the parser logic from the base module while preserving type
  ;; annotations.
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
             (unsafe-amb*
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
          [(_ : t1 (clauses ...) : t2 break:break-clause ... body ...+)
           (quasisyntax/loc stx
             (unsafe-amb*
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


(define-syntaxes (in-amb in-amb/do)
  ;; Thin wrappers around `in-amb*` and `in-amb*/do` that accept an
  ;; expression instead of a thunk.
  (let ()
    (define ((make derived-stx) stx)
      (syntax-parse stx
        #:datum-literals ()
        [(_ expr)
         (quasisyntax/loc stx
           (#,derived-stx (λ () expr)))]))
    (values (make #'in-amb*)
            (make #'in-amb*/do))))
