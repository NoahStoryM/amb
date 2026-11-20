#lang typed/racket/base/shallow

;; The typed `amb/base`: the effects of a search.

(require typed/racket/unsafe
         "private/types.rkt")

(provide Generatorof Merger
         with-amb-mark)

(unsafe-require/typed/provide amb/base
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
  [current-amb-merger (Parameter Merger)])

;; Use the typed procedure binding, just as the untyped syntax does.
(define-syntax-rule (with-amb-mark e)
  (call-with-amb-mark (λ () e)))
