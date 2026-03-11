#lang typed/racket/base/shallow

;; Typed wrapper around the base `amb` implementation.  All functions
;; are re-exported with precise types for use in Typed Racket programs.

(require (for-syntax racket/base syntax/parse)
         racket/sequence
         goto)
(require/typed racket/base
  [call-in-continuation
   (∀ (a ...)
      (case→
       (→ (→ a ... a Nothing) (→ (Values a ... a)) Nothing)
       (→ (→ Any * Nothing) (→ AnyValues) Nothing)))])

(provide amb amb* unsafe-amb* for/amb for*/amb in-amb in-amb/do)
(require/typed/provide amb/private/amb
  [empty-mutable-vector (Mutable-Vector)]
  [amb*        (∀ (a ...) (case→ (→                  Nothing) (→                   (→ (Values a ... a)) * (Values a ... a))))]
  [unsafe-amb* (∀ (a ...) (case→ (→ (Mutable-Vector) Nothing) (→ (Mutable-Vectorof (→ (Values a ... a)))  (Values a ... a))))]
  [in-amb*    (∀ (a ...) (→ (→ (Values a ... a)) (Sequenceof a ... a)))]
  [in-amb*/do (∀ (a ...) (→ (→ (Values a ... a)) (Sequenceof a ... a)))]
  [amb-prompt-tag Prompt-TagTop]
  [#:struct (exn:fail:contract:amb exn:fail:contract) ()]
  [raise-amb-error (→ Nothing)]
  [current-amb-prompt-tag    (Parameter Prompt-TagTop)]
  [current-amb-empty-handler (Parameter (→ Nothing))]
  [current-amb-shuffler (Parameter (→ Mutable-VectorTop Void))]
  [current-amb-rotator  (Parameter (→ SequenceTop Void))]
  [current-amb-maker    (Parameter (→ (Sequenceof Label)))]
  [current-amb-tasks    (Parameter (Sequenceof Label))]
  [current-amb-length   (Parameter (→ SequenceTop Index))]
  [current-amb-pusher   (Parameter (∀ (a) (→ (Sequenceof a) a Void)))]
  [current-amb-popper   (Parameter (∀ (a) (→ (Sequenceof a) a)))])


(define-syntax (amb stx)
  ;; Typed version of the `amb` macro.  Each expression is delayed and
  ;; processed by the underlying implementation.
  (syntax-parse stx
    [(_ expr ...)
     (syntax/loc stx
       (amb* (λ () expr) ...))]))


(define-syntaxes (for/amb for*/amb)
  ;; Typed versions of the `for/amb` and `for*/amb` macros.
  ;;
  ;; Differences from the untyped version
  ;; -------------------------------------
  ;; - `retry` initial value: `goto` instead of `#t`
  ;;   In the untyped version `retry` starts as `#t` (a boolean used
  ;;   as a "first-entry" sentinel) and later holds a `Label`
  ;;   continuation.  Typed Racket requires a single static type for
  ;;   each variable, so `retry` must be `Label` from the start.
  ;;   `goto` is used as the initial sentinel value; the first-entry
  ;;   check becomes `(eq? retry goto)` instead of the untyped `retry`.
  ;;   All subsequent logic is identical.

  (let ()
    (define-syntax-class type
      ;; Syntax class for parsing optional `(Values t ...) / t` return
      ;; type annotations.  Normalises multi-value `(Values t ...)` and
      ;; single-value `t` into a uniform `.ts` attribute (a syntax list
      ;; of the individual types).
      [pattern ((~or* (~literal Values)
                      (~literal values))
                t*:expr ...)
               #:with ts #'(t* ...)]
      [pattern t:expr
               #:with ts #'(t)])
    (define-splicing-syntax-class length-clause
      [pattern (~seq #:length n:expr (~optional (~seq #:fill fill:expr)))])
    (define-splicing-syntax-class break-clause
      [pattern (~seq (~or* #:break #:final) guard:expr)])
    (define (make-for/amb for/vector for)
      (define (parser stx)
        (syntax-parse stx
          #:datum-literals (: values Values)
          [(name
            : t1:type
            #:length n (~optional (~seq #:fill fill-expr))
            (clauses ...)
            : t2:type
            break:break-clause ...
            body ...+)
           #:with fill
           (if (attribute fill-expr)
               ;; Make sure `t2` is a subtype of `t1`
               #'(ann (λ () : t2 fill-expr) (→ t1))
               #'(ann amb* (→ Nothing)))
           (quasisyntax/loc stx
             (unsafe-amb*
              (let ([m n])
                (if (zero? m)
                    empty-mutable-vector
                    (#,for/vector
                     : (Mutable-Vectorof (→ t1))
                     #:length m #:fill fill
                     (clauses ...)
                     : (→ t2)
                     break ...
                     (ann (λ () : t2 body ...) (→ t1)))))))]
          [(_ : t1:type (clauses ...) : t2:type break:break-clause ... body ...+)
           #:with (t1* ...) #'t1.ts
           (quasisyntax/loc stx
             (call/cc
              (ann
               (λ (return)
                 (define retry : Label goto)
                 (define length (current-amb-length))
                 (define task* (current-amb-tasks))
                 (define task : Label (label (current-amb-prompt-tag)))
                 (cond
                   [(eq? retry goto)
                    ;; first entry
                    (set! retry task)
                    ((current-amb-pusher) task* task)
                    (goto (sequence-ref task* 0))]
                   [(not (eq? retry task))
                    (goto retry)])
                 (#,for (clauses ...) break ...
                  (define choice : Label (label (current-amb-prompt-tag)))
                  (unless (eq? retry choice)
                    (set! retry choice)
                    ((current-amb-rotator) task*)
                    (call-in-continuation return (λ () : t2 body ...))))
                 ;; no more alternatives
                 ((current-amb-popper) task*)
                 (define skip : Label (label (current-amb-prompt-tag)))
                 (unless (eq? retry skip)
                   (set! retry skip))
                 (when (zero? (length task*))
                   ((current-amb-empty-handler)))
                 (goto (sequence-ref task* 0)))
               (→ (→ t1* ... Nothing) t2))
              (current-amb-prompt-tag)))]
          ;; Syntactic normalisation:
          ;; Fold all optional-annotation variants into the fully-
          ;; annotated form `(name : t (clauses ...) : t ...)` before
          ;; passing to the cases above.  `AnyValues` is used as the
          ;; default type when no annotation is supplied.
          [(~or* (name : t0:type (~optional length:length-clause) (clauses ...) break:break-clause ... body ...+)
                 (name (~optional length:length-clause) (clauses ...) : t0:type break:break-clause ... body ...+)
                 (name (~optional length:length-clause) (clauses ...) break:break-clause ... body ...+))
           #:with t (if (attribute t0) #'t0 #'AnyValues)
           #:with (maybe-length ...) (if (attribute length) #'length #'())
           (parser (syntax/loc stx (name : t maybe-length ... (clauses ...) : t break ... body ...)))]))
      parser)
    (values (make-for/amb #'for/vector  #'for)
            (make-for/amb #'for*/vector #'for*))))


(define-syntaxes (in-amb in-amb/do)
  ;; Thin wrappers around `in-amb*` and `in-amb*/do` that accept an
  ;; expression instead of a thunk.
  (let ()
    (define ((make derived-stx) stx)
      (syntax-parse stx
        [(_ expr)
         (quasisyntax/loc stx
           (#,derived-stx (λ () expr)))]))
    (values (make #'in-amb*)
            (make #'in-amb*/do))))
