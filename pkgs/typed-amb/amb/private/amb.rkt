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
       (→ (→ a ... a ⊥) (→ (Values a ... a)) ⊥)
       (→ (→ Any * ⊥) (→ AnyValues) ⊥)))])
(define call/prompt call-with-continuation-prompt)
(define abort/cc abort-current-continuation)

(provide amb amb* unsafe-amb* for/amb for*/amb in-amb in-amb/do)
(require/typed/provide amb/private/amb
  [empty-mutable-vector (Mutable-Vector)]
  [fail (→* () (#:empty-handler (→ ⊥) #:tasks (Sequenceof Label) #:length (→ SequenceTop Index)) ⊥)]
  [amb*        (∀ (a ...) (case→ (→                  ⊥) (→                   (→ (Values a ... a)) * (Values a ... a))))]
  [unsafe-amb* (∀ (a ...) (case→ (→ (Mutable-Vector) ⊥) (→ (Mutable-Vectorof (→ (Values a ... a)))  (Values a ... a))))]
  [in-amb*    (∀ (a ...) (→ (→ (Values a ... a)) (Sequenceof a ... a)))]
  [in-amb*/do (∀ (a ...) (→ (→ (Values a ... a)) (Sequenceof a ... a)))]
  [amb-prompt-tag    Prompt-TagTop]
  [return-prompt-tag Prompt-TagTop]
  [#:struct (exn:fail:contract:amb exn:fail:contract) ()]
  [raise-amb-error (→ ⊥)]
  [current-amb-prompt-tag    (Parameter Prompt-TagTop)]
  [current-amb-empty-handler (Parameter (→ ⊥))]
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
  (let ()
    (define-syntax-class type
      ;; Syntax class for parsing optional `(Values t ...)` / `t`
      ;; return type annotations.  Normalises multi-value
      ;; `(Values t ...)` and single-value `t` into a uniform `.ts`
      ;; attribute (a syntax list of the individual types).
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
               #'(ann amb* (→ ⊥)))
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
                     (define (alt) : t2 body ...)
                     (ann alt (→ t1)))))))]
          ;; Differences from the untyped version
          ;; ------------------------------------
          ;; The untyped version uses `call/prompt` and `abort/cc` to
          ;; return results from the loop. However, Typed Racket's
          ;; `Prompt-Tagof` and `call/prompt` type signatures do not
          ;; support multiple values.
          ;;
          ;; `call/prompt` :
          ;; - : (∀ (a b d c ...)
          ;;        (case→
          ;;         (→ (→ b) (Prompt-Tagof b (→ (→ d) d)) (∪ b d))
          ;;         (→ (→ b) (Prompt-Tagof b (→ c ... c d)) (→ c ... c d) (∪ b d))
          ;;         (→ (→ b) Any)))
          ;;
          ;; Type variables (like `b` and `d` in the TR signature)
          ;; only instantiate to single types, not `(Values t ...)`.
          ;;
          ;; To work around this limitation, the typed version uses
          ;; delimited `call/cc` (bounded by `current-amb-prompt-tag`)
          ;; to explicitly capture the return continuation.
          [(_ : t1:type (clauses ...) : t2:type break:break-clause ... body ...+)
           #:with (t1* ...) #'t1.ts
           (quasisyntax/loc stx
             (call/cc
              (ann
               (λ (return)
                 (define length (current-amb-length))
                 (define task* (current-amb-tasks))
                 (define first? : Boolean #t)
                 (define retry : (∪ False (¬ False)) #f)
                 (define task : Label (label (current-amb-prompt-tag)))
                 (let ([retry retry])
                   (when retry (goto retry #f)))
                 (when first?
                   (set! first? #f)
                   ((current-amb-pusher) task* task)
                   (goto (sequence-ref task* 0)))

                 (#,for (clauses ...) break ...
                  (define choice : (∪ False (¬ False)) (label (current-amb-prompt-tag)))
                  (when choice
                    (define (alt) : t2 body ...)
                    (set! retry choice)
                    ((current-amb-rotator) task*)
                    (call-in-continuation return alt)))

                 ((current-amb-popper) task*)
                 (define skip : (∪ False (¬ False)) (label (current-amb-prompt-tag)))
                 (when skip (set! retry skip))
                 (fail #:tasks task*
                       #:length length))
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
