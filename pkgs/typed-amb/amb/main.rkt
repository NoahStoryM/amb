#lang typed/racket/base/shallow

;; ===================================================================
;; Public API for `typed-amb` library
;; ===================================================================
;;
;; This module re-exports the typed core from `typed/amb/private/amb`
;; and adds `define-sequence-syntax` wrappers so that `in-amb` and
;; `in-amb*` work efficiently inside `for` comprehensions.
;;
;; The dual-role pattern is the same as in the untyped `amb/main`:
;;
;;   - As an expression (outside `for`): returns a lazy stream
;;     via `in-amb*`.
;;
;;   - Inside a `for` clause: expands directly to `in-amb*/do`
;;     (a `make-do-sequence`-based sequence), bypassing stream
;;     construction entirely.
;;
;; Unlike the untyped version, no runtime `check-thk` validation
;; is needed — Typed Racket's type system enforces the thunk
;; contract statically.

(require "private/amb.rkt"
         (for-syntax racket/base syntax/parse))

(provide make-amb
         amb for/amb for*/amb
         (rename-out [*in-amb in-amb] [*in-amb* in-amb*])
         in-amb/do in-amb*/do
         current-amb-depth-first?
         current-amb-fair?
         current-amb-shuffler)


;; ---- in-amb* (function form) ----

;; `*in-amb*` is the public binding for `in-amb*`.
;;
;; As an expression: falls through to `in-amb*`, returning a
;; lazy stream.
;;
;; Inside a `for` clause: dispatches to `in-amb*/do` for direct
;; `make-do-sequence` iteration, avoiding stream overhead.
(define-sequence-syntax *in-amb*
  (λ () #'in-amb*)
  (λ (stx)
    (syntax-parse stx
      [[(id:id ...) (_ expr)]
       (syntax/loc stx
         [(id ...)
          (in-amb*/do expr)])])))


;; ---- in-amb (macro form) ----

;; Transformer for the expression position: wraps the body in a thunk
;; and calls `in-amb*`.
(define-for-syntax (in-amb stx)
  (syntax-parse stx
    [(_ expr)
     (syntax/loc stx
       (in-amb* (λ () expr)))]))

;; `*in-amb` is the public binding for `in-amb`.
;;
;; As an expression: expands via `in-amb` above, returning a
;; lazy stream.
;;
;; Inside a `for` clause: wraps the body in a thunk
;; and dispatches directly to `in-amb*/do`.
(define-sequence-syntax *in-amb
  in-amb
  (λ (stx)
    (syntax-parse stx
      [[(id:id ...) (_ expr)]
       (syntax/loc stx
         [(id ...)
          (in-amb*/do (λ () expr))])])))


;; ---- in-amb/do (macro form, sequence only) ----

;; Always produces a `make-do-sequence` sequence (never a stream).
;; Intended for cases where only `for`-style iteration is needed.
(define-syntax (in-amb/do stx)
  (syntax-parse stx
    [(_ expr)
     (syntax/loc stx
       (in-amb*/do (λ () expr)))]))
