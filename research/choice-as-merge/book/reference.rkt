#lang typed/racket/base

;; The examples of the book on miniKanren-typed's typed/miniKanren/trs2,
;; the stream implementation of the book: every example is `same`.  This
;; module is here for the running time.

(require racket/include
         (except-in racket/match ==)
         (only-in racket/list make-list)
         (for-syntax racket/base racket/path)
         typed/miniKanren/trs2)

(require/typed "record.rkt"
  [record! (→ String (→ Any) Any Void)]
  [finish! (→ String (Option Path-String) Void)])

(include "examples.rktl")

(finish! "reference" (build-path "results" "reference.rktd"))
