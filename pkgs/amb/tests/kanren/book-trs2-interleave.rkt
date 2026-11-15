#lang typed/racket/base/no-check

;; The second edition, with amb-interleave as the merger of every disjunction;
;; conjunction is function composition.

(require racket/include
         amb/merge)

(current-amb-merger amb-interleave)
(include "trs2.rktl")
(check-book "TRS2, amb-interleave" '((same . 283) (same-answers . 2)))
