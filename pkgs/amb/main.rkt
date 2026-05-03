#lang racket/base

;; ===================================================================
;; Public API for the `amb` library
;; ===================================================================
;;
;; This module re-exports the core primitives from the private
;; implementation and wraps them with contracts, input validation,
;; and `define-sequence-syntax` optimizations for use in `for`
;; comprehensions.
;;
;; The two layers of sequence syntax (`in-amb` and `in-amb*`)
;; each have a dual role:
;;
;;   - As an expression (outside `for`): returns a lazy stream
;;     via `in-amb*` from the private module.
;;
;;   - Inside a `for` clause: expands directly to the more efficient
;;     `in-amb*/do` (a `make-do-sequence`-based sequence) to avoid
;;     the overhead of stream construction.

(require (for-syntax racket/base syntax/parse)
         racket/contract/base
         racket/stream
         racket/mutability
         (rename-in "private/amb.rkt"
                    [in-amb*    -in-amb*]
                    [in-amb*/do -in-amb*/do])
         (contract-in "private/amb.rkt"
                      [in-amb*    (-> (-> any) stream?)]
                      [in-amb*/do (-> (-> any) sequence?)]))

(provide amb
         for/amb for*/amb
         (rename-out [*in-amb in-amb] [*in-amb* in-amb*])
         in-amb/do in-amb*/do
         (contract-out
          [make-amb (-> (-> boolean?) (-> any) any)]
          [current-amb-depth-first? (parameter/c boolean?)]
          [current-amb-fair?        (parameter/c boolean?)]
          [current-amb-shuffler     (parameter/c (-> mutable-vector? void?))]))


;; ---- in-amb* (function form) ----

;; Validate that the argument is a zero-arity procedure before
;; calling the private `in-amb*`.  If it is not, delegate to the
;; contracted `in-amb*` which will raise a proper contract error.
(define (check-thk thk)
  (unless (and (procedure? thk) (procedure-arity-includes? thk 0))
    (in-amb* thk)))

;; `*in-amb*` is the public binding for `in-amb*`.
;;
;; As an expression: falls through to the contracted `in-amb*`,
;; returning a lazy stream.
;;
;; Inside a `for` clause: validates the thunk, then dispatches to
;; `-in-amb*/do` for direct `make-do-sequence` iteration.
(define-sequence-syntax *in-amb*
  (λ () #'in-amb*)
  (λ (stx)
    (syntax-parse stx
      [[(id:id ...) (_ expr)]
       (syntax/loc stx
         [(id ...)
          (let ([thk expr])
            (check-thk thk)
            (-in-amb*/do thk))])])))


;; ---- in-amb (macro form) ----

;; Transformer for the expression position: wraps the body in a thunk
;; and calls the private `in-amb*`.
(define-for-syntax (in-amb stx)
  (syntax-parse stx
    [(_ expr)
     (syntax/loc stx
       (-in-amb* (λ () expr)))]))

;; `*in-amb` is the public binding for `in-amb`.
;;
;; As an expression: expands via `in-amb` above, returning
;; a lazy stream.
;;
;; Inside a `for` clause: wraps the body in a thunk and dispatches
;; directly to `-in-amb*/do`, bypassing stream construction entirely.
(define-sequence-syntax *in-amb
  in-amb
  (λ (stx)
    (syntax-parse stx
      [[(id:id ...) (_ expr)]
       (syntax/loc stx
         [(id ...)
          (-in-amb*/do (λ () expr))])])))


;; ---- in-amb/do (macro form, sequence only) ----

;; Unlike `in-amb`, this form always produces a `make-do-sequence`
;; sequence (never a stream).  It is intended for use cases where
;; the caller only needs `for`-style iteration and wants to avoid
;; the overhead of lazy stream construction.
(define-syntax (in-amb/do stx)
  (syntax-parse stx
    [(_ expr)
     (syntax/loc stx
       (in-amb*/do (λ () expr)))]))
