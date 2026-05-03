#lang typed/racket/base/shallow

;; ===================================================================
;; Typed Front-End for `amb`
;; ===================================================================
;;
;; This module provides the typed interface to the untyped core in
;; `amb/private/amb`.  It uses `require/typed/provide` to re-export
;; the untyped bindings with Typed Racket annotations, and defines
;; typed versions of the syntactic forms (`amb`, `for/amb`, `in-amb`,
;; etc.) that cannot be handled by `require/typed` alone.
;;
;; Uses `typed/racket/base/shallow` for lower contract overhead at the
;; typed-untyped boundary.

(require (for-syntax racket/base syntax/parse))

(provide amb
         for/amb for*/amb
         in-amb  in-amb/do)


;; ---- Typed Re-Exports from the Untyped Core ----
;;
;; Each binding from `amb/private/amb` is annotated with its type.
;;
;; `amb*` uses `case→` to distinguish the zero-argument case
;; (always diverges → ⊥) from the variadic case (chooses among
;; thunks → their shared return type).
;;
;; `unsafe-amb*` similarly distinguishes an empty mutable vector
;; (diverges) from a vector of thunks.  The vector slots are
;; overwritten with `amb*` as a consumed-slot sentinel; since
;; `amb*` is itself a procedure, this is type-compatible with
;; `(Mutable-Vectorof (→ ...))`.
;;
;; `make-amb-tree` returns `(Values)` (zero values) because the first
;; call shifts out and never returns; on resumption the continuation
;; produces no useful value — the amb tree's state is managed
;; internally.
(require/typed/provide amb/private/amb
  [thunks
   (∀ (a ...)
      (case→ (→ (Mutable-Vector))
             (→ (→ (Values a ... a))
                (→ (Values a ... a))
                *
                (Mutable-Vectorof (→ (Values a ... a))))))]
  [unsafe-amb*
   (∀ (a ...)
      (case→ (¬ (Mutable-Vector))
             (→ (Mutable-Vectorof (→ (Values a ... a)))
                (Values a ... a))))]
  [make-amb
   (∀ (a ...)
      (→ (→ Boolean) (→ (Values a ... a))
         (Values a ... a)))]
  [in-amb*
   (∀ (a ...)
      (case→ (→ (→ ⊥)
                (∀ (b ...) (Sequenceof b ... b)))
             (→ (→ (Values a ... a))
                (Sequenceof a ... a))))]
  [in-amb*/do
   (∀ (a ...)
      (case→ (→ (→ ⊥)
                (∀ (b ...) (Sequenceof b ... b)))
             (→ (→ (Values a ... a))
                (Sequenceof a ... a))))]
  [current-amb-depth-first? (Parameter Boolean)]
  [current-amb-fair?        (Parameter Boolean)]
  [current-amb-shuffler     (Parameter (→ Mutable-VectorTop Void))]
  [unsafe-fail (→ ⊥)]
  [fail        (→ ⊥)]
  [make-amb-tree (→ (Values))])

(require/typed/provide amb/private/data/mutable-vector
  [empty-mutable-vector (Mutable-Vector)])


;; ---- Aliases ----

(define-for-syntax (append* v**) (apply append v**))

(define-type ⊥ Nothing)
(define-type (¬ a) (→ a ⊥))

(define call/prompt call-with-continuation-prompt)
(define abort/cc abort-current-continuation)


;; ---- amb Syntax ----

;; Wraps each expression in a thunk and delegates to `unsafe-amb*`.
;; Identical to the untyped version; defined here so that it
;; expands in a typed context.
(define-syntax (amb stx)
  (syntax-parse stx
    [(_)
     (syntax/loc stx (fail))]
    [(_ expr* ...)
     (syntax/loc stx (unsafe-amb* (thunks (λ () expr*) ...)))]))


;; ---- for/amb: Nondeterministic Choice over Sequences ----
;;
;; The typed `for/amb` is substantially more complex than the
;; untyped version because Typed Racket needs explicit type
;; annotations to check the vector and thunk types.
;;
;; Three syntax patterns are supported:
;;
;;   (for/amb : T₁ #:length n (clauses ...) : T₂ body ...)
;;     Known iteration count — collects thunks into a typed
;;     `(Mutable-Vectorof (→ T₁))` via `for/vector`, then
;;     delegates to `unsafe-amb*`.  `T₁` is the outer result
;;     type; `T₂` is the body type (must be a subtype of `T₁`).
;;
;;   (for/amb : T₁ (clauses ...) : T₂ body ...)
;;     Unknown iteration count — creates a fresh prompt tag
;;     typed as `(Prompt-Tagof (→ T₁) ...)` and uses `call/cc`
;;     + `abort/cc` to yield body thunks one at a time.  The
;;     fresh tag (rather than the shared `return-prompt-tag`)
;;     is necessary because each `for/amb` site may have a
;;     different result type `T₁`.
;;
;;   (for/amb (clauses ...) body ...)
;;     No type annotations — defaults to `AnyValues` for both
;;     `T₁` and `T₂`, and re-dispatches to one of the above.
;;
;; The `type` syntax class normalizes `(Values T₁ T₂ ...)` and
;; bare `T` into a uniform `ts` attribute for splicing.
;;
;; Two compilation strategies:
;;
;; 1. With `#:length` — Collect body thunks into a typed vector via
;;    `for/vector`, then delegate to `unsafe-amb*`.  The optional
;;    `#:fill` thunk is annotated as `(→ T₁)` to enforce that it is a
;;    subtype of the outer result type.  Without `#:fill`, `amb*` is
;;    used as the fill value
;;    (a procedure, so type-compatible with the vector element type).
;;
;; 2. Without `#:length` — Create a fresh prompt tag typed for this
;;    specific result type.  `retry` is typed as `(∪ False (¬ False))`
;;    — either #f (not yet captured) or an escape continuation that
;;    accepts #f to advance the loop.
;;
;;    `(let ([retry retry])` re-binds `retry` in a local scope so that
;;    Typed Racket can narrow its type after `when`.

(define-syntaxes (for/amb for*/amb)
  (let ()
    (define-syntax-class type
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
            break-expr:break-clause ...
            body ...+)
           #:with fill
           (if (attribute fill-expr)
               #'(ann (λ () : t2 fill-expr) (→ t1)) ; ensure `t2 <: t1`
               #'fail)
           #:do
           [(define break
              (if (attribute break-expr)
                  (append* (map syntax->list
                                (syntax->list #'(break-expr ...))))
                  '()))]
           (quasisyntax/loc stx
             (let ([m n])
               (if (zero? m)
                   (fail)
                   (unsafe-amb*
                    (#,for/vector
                     : (Mutable-Vectorof (→ t1))
                     #:length m #:fill fill
                     (clauses ...)
                     : (→ t2)
                     #,@break
                     (define (alt) : t2 body ...)
                     (ann alt (→ t1)))))))]
          [(_ : t1:type (clauses ...) : t2:type break-expr:break-clause ... body ...+)
           #:do
           [(define break
              (if (attribute break-expr)
                  (append* (map syntax->list
                                (syntax->list #'(break-expr ...))))
                  '()))]
           (quasisyntax/loc stx
             ((let ([prompt-tag ((inst make-continuation-prompt-tag
                                       (→ t1) (→ (→ t1) (→ t1))))])
                (call/prompt
                 (λ ()
                   (: retry (∪ False (¬ False)))
                   (define retry #f)
                   (make-amb-tree)
                   (let ([retry retry])
                     (when retry (retry #f)))
                   (#,for (clauses ...) #,@break
                    (define choice
                      (call/cc (inst values (¬ False)) prompt-tag))
                    (when choice
                      (define (alt) : t2 body ...)
                      (set! retry choice)
                      (abort/cc prompt-tag alt)))
                   (set! retry #f)
                   (unsafe-fail))
                 prompt-tag
                 (inst values (→ t1))))))]
          [(~or* (name : t0:type (~optional length:length-clause) (clauses ...) break-expr:break-clause ... body ...+)
                 (name (~optional length:length-clause) (clauses ...) : t0:type break-expr:break-clause ... body ...+)
                 (name (~optional length:length-clause) (clauses ...) break-expr:break-clause ... body ...+))
           #:with t (if (attribute t0) #'t0 #'AnyValues)
           #:with (maybe-length ...) (if (attribute length) #'length #'())
           #:do
           [(define break
              (if (attribute break-expr)
                  (append* (map syntax->list
                                (syntax->list #'(break-expr ...))))
                  '()))]
           (parser
            (quasisyntax/loc stx
              (name : t maybe-length ... (clauses ...) : t #,@break body ...)))]))
      parser)
    (values (make-for/amb #'for/vector  #'for)
            (make-for/amb #'for*/vector #'for*))))


;; ---- in-amb / in-amb/do Syntax ----

;; Wrap the expression in a thunk and delegate to `in-amb*` or
;; `in-amb*/do`.  Identical to the untyped versions; defined here
;; so that they expand in a typed context.
(define-syntaxes (in-amb in-amb/do)
  (let ()
    (define ((make derived-stx) stx)
      (syntax-parse stx
        [(_ expr)
         (quasisyntax/loc stx
           (#,derived-stx (λ () expr)))]))
    (values (make #'in-amb*)
            (make #'in-amb*/do))))
