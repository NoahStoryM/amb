#lang typed/racket/base/no-check

;; The examples of the book on ../kanren.rkt.  The variant is chosen by
;; the environment variable VARIANT:
;;
;;   interleave         disj2 merges with amb-interleave; conj2 composes
;;   interleave+local   the same, and conj2 searches its first goal locally
;;   level              disj2 merges with amb-level; conj2 composes
;;   alternate          disj2 merges with amb-alternate; conj2 composes

(require racket/include
         (except-in racket/match ==)
         (only-in racket/list make-list)
         (for-syntax racket/base racket/path)
         "../kanren.rkt"
         "../kernel.rkt"
         "record.rkt")

(define variant (or (getenv "VARIANT") "interleave"))

(case variant
  [("interleave") (current-amb-merge amb-interleave)]
  [("interleave+local") (current-amb-merge amb-interleave) (current-conj-local? #t)]
  [("level") (current-amb-merge amb-level)]
  [("alternate") (current-amb-merge amb-alternate)]
  [else (error 'book "unknown VARIANT: ~a" variant)])

(include "examples.rktl")

(finish! variant (build-path "results" (string-append variant ".rktd")))
