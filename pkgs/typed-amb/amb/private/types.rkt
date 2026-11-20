#lang typed/racket/base/shallow

(provide Generatorof Merger ⊥)

;; A merger preserves the complete list of an element's return values.
;; The dotted variables fix that list's arity at each instantiation;
;; `list?` therefore distinguishes an answer in typed implementations.
;; The typed boundary trusts the untyped implementation to preserve
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
