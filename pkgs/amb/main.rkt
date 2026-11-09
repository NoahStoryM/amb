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
                      [in-amb*    (->* ((-> any)) ((or/c #f list?)) stream?)]
                      [in-amb*/do (->* ((-> any)) ((or/c #f list?)) sequence?)]))

(provide amb
         for/amb for*/amb
         (rename-out [*in-amb in-amb] [*in-amb* in-amb*])
         in-amb/do in-amb*/do
         ;; RESEARCH (ambKanren #14)
         amb-fail amb-empty
         (contract-out
          [make-amb (-> (-> boolean?) (-> any) any)]
          [current-amb-depth-first? (parameter/c boolean?)]
          [current-amb-fair?        (parameter/c boolean?)]
          [current-amb-shuffler     (parameter/c (-> mutable-vector? void?))]))


;; ---- in-amb* (function form) ----

;; Validate the arguments before calling the private `in-amb*/do`:
;; `thk` must be a zero-arity procedure and `vs` (the empty values)
;; must be #f or a list.  The call to the contracted `in-amb*` is only
;; for error reporting: when an argument is invalid its contract raises
;; the proper error (with blame) before the body runs, so nothing is
;; executed twice.  When the arguments are valid, nothing is called.
(define (check-args thk [vs #f])
  (unless (and (procedure? thk)
               (procedure-arity-includes? thk 0)
               (or (not vs) (list? vs)))
    (in-amb* thk vs)))

;; `*in-amb*` is the public binding for `in-amb*`.
;;
;; As an expression: falls through to the contracted `in-amb*`,
;; returning a lazy stream.
;;
;; Inside a `for` clause: validates the arguments, then dispatches to
;; `-in-amb*/do` for direct `make-do-sequence` iteration.
(define-sequence-syntax *in-amb*
  (λ () #'in-amb*)
  (λ (stx)
    (syntax-parse stx
      [[(id:id ...) (_ expr)]
       (syntax/loc stx
         [(id ...)
          (let ([thk expr])
            (check-args thk)
            (-in-amb*/do thk))])]
      [[(id:id ...) (_ expr vs-expr)]
       (syntax/loc stx
         [(id ...)
          (let ([thk expr] [vs vs-expr])
            (check-args thk vs)
            (-in-amb*/do thk vs))])])))


;; ---- in-amb (macro form) ----

;; Transformer for the expression position: wraps the body in a thunk
;; and calls the private `in-amb*`.
(define-for-syntax (in-amb stx)
  (syntax-parse stx
    [(_ expr)
     (syntax/loc stx
       (-in-amb* (λ () expr)))]
    [(_ expr #:empty-values vs)
     (syntax/loc stx
       (in-amb* (λ () expr) vs))]))

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
          (-in-amb*/do (λ () expr))])]
      [[(id:id ...) (_ expr #:empty-values vs)]
       (syntax/loc stx
         [(id ...)
          (in-amb*/do (λ () expr) vs)])])))


;; ---- in-amb/do (macro form, sequence only) ----

;; Unlike `in-amb`, this form always produces a `make-do-sequence`
;; sequence (never a stream).  It is intended for use cases where
;; the caller only needs `for`-style iteration and wants to avoid
;; the overhead of lazy stream construction.
(define-syntax (in-amb/do stx)
  (syntax-parse stx
    [(_ expr)
     (syntax/loc stx
       (in-amb*/do (λ () expr)))]
    [(_ expr #:empty-values vs)
     (syntax/loc stx
       (in-amb*/do (λ () expr) vs))]))
