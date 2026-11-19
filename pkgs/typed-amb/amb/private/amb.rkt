#lang typed/racket/base/shallow

;; ===================================================================
;; Typed Front-End for `amb`
;; ===================================================================
;;
;; This module gives types to the public bindings of `amb/base`,
;; `amb/merge` and `amb/main`, and defines typed versions of the
;; syntactic forms (`amb`, `for/amb`, `in-amb`, ...), which cannot be
;; imported with `require/typed`.  Runtime helpers shared with amb/main
;; are imported from amb/private/choice; their implementations stay in
;; the untyped package.
;;
;; The untyped implementation is a trusted boundary: its own checks
;; enforce the generator protocol, and these declarations supply static
;; types without installing another layer of run-time contracts.

(require typed/racket/unsafe
         (for-syntax racket/base syntax/parse)
         racket/vector)

(provide Generatorof Merger
         amb amb-local
         for/amb for*/amb)


;; ---- The generator protocol ----
;;
;; A merger preserves the complete list of an element's return values.
;; The dotted variables fix that list's arity at each instantiation;
;; `list?` therefore distinguishes an answer in typed implementations.
;;
;; Unsafe imports below trust our untyped implementation to preserve
;; these types. Protocol checks alone do not establish parametricity
;; for arbitrary untyped mergers supplied by clients.
(define-type (Generatorof a)
  (→ (Values (∪ a #f Positive-Integer) (Generatorof a))))
(define-type Merger
  (∀ (a ...)
     (→ (Generatorof (List a ... a))
        (Generatorof (List a ... a))
        (Generatorof (List a ... a)))))

(define-type ⊥ Nothing)


;; ---- Typed Re-Exports ----

(unsafe-require/typed/provide amb/merge
  [amb-append     Merger]
  [amb-interleave Merger]
  [amb-alternate  Merger]
  [amb-level      Merger])

;; `in-amb*` and `in-amb*/do` take an optional pause handler; the
;; sequence type is the union of the body's type and the handler's.
(unsafe-require/typed/provide amb
  [make-amb
   (∀ (a ...)
      (→ (Generatorof (List a ... a))
         (Values a ... a)))]
  [amb-generate*
   (∀ (a ...)
      (→ (→ (Values a ... a))
         (Values (∪ (List a ... a) #f Positive-Integer)
                 (Generatorof (List a ... a)))))]
  [amb-fail  (→ ⊥)]
  [amb-pause (→* () (Natural) Void)]
  [call-with-amb-mark
   (∀ (a ...)
      (case→ (→ (→ ⊥) ⊥)
             (→ (→ (Values a ... a)) (Values a ... a))))]
  [current-amb-merger   (Parameter Merger)]
  [current-amb-shuffler (Parameter (→ Mutable-VectorTop Void))]
  [sequence->amb
   (∀ (a ...)
      (→ (Sequenceof a ... a)
         (Values a ... a)))]
  [amb-local*
   (∀ (a ...)
      (→ (→ (Values a ... a))
         (Values a ... a)))]
  [in-amb*
   ;; The type variables c and d come first, so an explicit
   ;; instantiation must supply them: (inst in-amb* c d a ...).
   (∀ (c d a ...)
      (case→ (→* ((→ ⊥)) (False)
                 (∀ (b ...) (Sequenceof b ... b)))
             (→* ((→ (Values a ... a))) (False)
                 (Sequenceof a ... a))
             (→ (→ c) (Option (→ d)) (Sequenceof (∪ c d)))
             (→ (→ (Values a ... a)) (Option (→ (Values a ... a)))
                (Sequenceof a ... a))))]
  [in-amb*/do
   (∀ (c d a ...)
      (case→ (→* ((→ ⊥)) (False)
                 (∀ (b ...) (Sequenceof b ... b)))
             (→* ((→ (Values a ... a))) (False)
                 (Sequenceof a ... a))
             (→ (→ c) (Option (→ d)) (Sequenceof (∪ c d)))
             (→ (→ (Values a ... a)) (Option (→ (Values a ... a)))
                (Sequenceof a ... a))))])


;; ---- Shared runtime support for the syntax ----

(unsafe-require/typed/provide amb/private/choice
  [thunks
   (∀ (a ...)
      (→ (→ (Values a ... a))
         (→ (Values a ... a))
         *
         (Mutable-Vectorof (→ (Values a ... a)))))]
  [choose
   (∀ (a ...)
      (→ (Mutable-Vectorof (→ (Values a ... a)))
         (→ (Values a ... a))))]
  [choose-none (→ ⊥)]
  [make-choice-generator
   (∀ (a) (→ (→ (→ a Void) Void) (Generatorof (List a))))])

;; Use the typed procedure binding, just as the untyped syntax does.
(provide with-amb-mark)
(define-syntax-rule (with-amb-mark e)
  (call-with-amb-mark (λ () e)))


;; ---- amb Syntax ----

(define-syntax amb
  (syntax-rules ()
    [(_)
     (choose-none)]
    [(_ expr* ...)
     ((choose (thunks (λ () expr*) ...)))]))


;; ---- amb-local Syntax ----

;; Identical to the untyped version; defined here so that it expands in
;; a typed context.
(define-syntax-rule (amb-local expr)
  (amb-local* (λ () expr)))


;; ---- for/amb: Nondeterministic Choice over Sequences ----
;;
;; Three syntax patterns are supported:
;;
;;   (for/amb : T₁ #:length n (clauses ...) : T₂ body ...)
;;     The alternatives are collected first, as by `for/vector`: at
;;     most n of them.  `T₁` is the outer result type; `T₂` is the body
;;     type (must be a subtype of `T₁`).
;;
;;   (for/amb : T₁ (clauses ...) : T₂ body ...)
;;     The iterations go on as the search does.
;;
;;   (for/amb (clauses ...) body ...)
;;     No type annotations — defaults to `AnyValues` for both `T₁` and
;;     `T₂`, and re-dispatches to one of the above.
;;
;; Result annotations, including `(Values T₁ T₂ ...)`, are put directly
;; on the body thunks and their vector or producer.
;;
;; Two compilation strategies:
;;
;; 1. With `#:length` — Collect the body thunks into a typed vector via
;;    `for/vector`, counting the iterations; the slots that no
;;    iteration fills hold `amb-fail` and are cut off.  With `#:fill`,
;;    they are alternatives; the fill thunk is annotated as `(→ T₁)`.
;;
;; 2. Without `#:length` — As in the untyped `for/amb`, the loop is a
;;    coroutine, which is the generator of `make-amb`.  The
;;    loop runs under a prompt of its own; at each iteration it captures
;;    its continuation as `retry` and aborts to the prompt with the body
;;    thunk.  The next call of the generator re-enters the loop through
;;    `retry`.

(define-syntaxes (for/amb for*/amb)
  (let ()
    (define-splicing-syntax-class length-clause
      [pattern (~seq #:length n:expr (~optional (~seq #:fill fill:expr)))])
    (define-splicing-syntax-class break-clause
      [pattern (~seq (~or* #:break #:final) guard:expr)])
    (define ((make-for/amb for/vector for) stx)
      (syntax-parse stx
        #:datum-literals (:)
        [(_ : t1:expr (clause ...) : t2:expr break:break-clause ... body ...+)
         #:with (break* ...) #'((~@ . break) ...)
         (quasisyntax/loc stx
           (((inst make-amb (→ t1))
             ((inst make-choice-generator (→ t1))
              (λ (yield)
                (#,for (clause ... break* ...)
                  (yield (ann (λ () : t2 body ...) (→ t1)))))))))]
        [(_ : t1:expr #:length n (clause ...) : t2:expr
            break:break-clause ... body ...+)
         #:with (break* ...) #'((~@ . break) ...)
         (quasisyntax/loc stx
           (let ([m (with-amb-mark n)])
             (if (eqv? m 0)
                 (choose-none)
                 ((choose
                   (with-amb-mark
                    (let* ([count : Integer 0]
                           [thk* (#,for/vector
                                  : (Mutable-Vectorof (→ t1))
                                  #:length m #:fill amb-fail
                                  (clause ... break* ...)
                                  (set! count (add1 count))
                                  (ann (λ () : t2 body ...) (→ t1)))])
                      (if (= count (vector-length thk*))
                          thk*
                          (vector-copy thk* 0 count)))))))))]
        [(_ : t1:expr #:length n #:fill fill-expr
            (clause ...) : t2:expr break:break-clause ... body ...+)
         #:with (break* ...) #'((~@ . break) ...)
         (quasisyntax/loc stx
           (let ([m (with-amb-mark n)])
             (if (eqv? m 0)
                 (choose-none)
                 ((choose
                   (with-amb-mark
                    (#,for/vector
                     : (Mutable-Vectorof (→ t1))
                     #:length m #:fill (ann (λ () : t2 fill-expr) (→ t1))
                     (clause ... break* ...)
                     (ann (λ () : t2 body ...) (→ t1)))))))))]
        [(~or* (name : t0:expr (~optional length:length-clause) (clause ...) break:break-clause ... body ...+)
               (name (~optional length:length-clause) (clause ...) : t0:expr break:break-clause ... body ...+)
               (name (~optional length:length-clause) (clause ...) break:break-clause ... body ...+))
         #:with t (if (attribute t0) #'t0 #'AnyValues)
         #:with (maybe-length ...) (if (attribute length) #'length #'())
         #:with (break* ...) #'((~@ . break) ...)
         ((make-for/amb for/vector for)
          (quasisyntax/loc stx
            (name : t maybe-length ... (clause ...) : t break* ... body ...)))]))
    (values (make-for/amb #'for/vector  #'for)
            (make-for/amb #'for*/vector #'for*))))
