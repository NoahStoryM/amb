#lang typed/racket/base/no-check

;; The second edition, with amb-interleave, and the conjunction of the
;; book: the first goal is searched on its own, as the bind of a stream
;; does.

(require racket/include
         amb/merge)

(current-amb-merger amb-interleave)
(current-conj-local? #t)
(include "trs2.rktl")
(check-book "TRS2, amb-interleave, conjunction as bind" '((same . 285)))
