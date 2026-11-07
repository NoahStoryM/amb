#lang typed/racket/base/shallow

;; Experimental continuation boundary, separate from the public front-end.
;; No recursive event structures or state-accepting bind callbacks cross it.
(require/typed/provide amb/private/hierarchical
  [call-with-local-search
   (∀ (a ...)
      (case→ (→ (→ (Values a ... a)) (Values a ... a))
             (→ (→ (Values a ... a)) (Option Natural) (Values a ... a))))]
  [call-with-local-default
   (∀ (a ...)
      (case→ (→ (→ (Values a ... a)) (→ (Values a ... a)) (Values a ... a))
             (→ (→ (Values a ... a)) (→ (Values a ... a))
                (Option Natural) (Values a ... a))))]
  [suspend (→ Void)])
