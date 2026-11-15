#lang typed/racket/base/no-check

;; The second edition, with amb-alternate as the merger of every disjunction;
;; conjunction is function composition.

(require racket/include
         amb/merge)

(current-amb-merger amb-alternate)
(include "trs2.rktl")
(check-book "TRS2, amb-alternate" '((same . 278) (same-answers . 7)))
