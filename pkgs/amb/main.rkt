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
                    [in-amb*/do -in-amb*/do]))

;; Module-boundary contracts (rather than `contract-in`): the module
;; that uses `in-amb*` / `in-amb*/do` -- including through the `for`
;; clause expansion below -- is the one blamed for a bad argument.
(define-module-boundary-contract in-amb*
  -in-amb*    (->* ((-> any)) ((-> any) #:frontier (or/c #f amb-frontier?)) stream?))
(define-module-boundary-contract in-amb*/do
  -in-amb*/do (->* ((-> any)) ((-> any) #:frontier (or/c #f amb-frontier?)) sequence?))

(provide amb
         for/amb for*/amb
         amb/local for/amb/local for*/amb/local
         (rename-out [*in-amb in-amb] [*in-amb* in-amb*])
         in-amb/do in-amb*/do
         amb-fail
         (contract-out
          [make-amb (-> (-> boolean?) (-> any) any)]
          [current-amb-depth-first? (parameter/c boolean?)]
          [current-amb-fair?        (parameter/c boolean?)]
          [current-amb-shuffler     (parameter/c (-> mutable-vector? void?))]
          [make-amb-frontier          (->* () ((-> any)) amb-frontier?)]
          [amb-frontier?              (-> any/c boolean?)]
          [amb-frontier-length        (-> amb-frontier? exact-nonnegative-integer?)]
          [amb-frontier-empty-handler (-> amb-frontier? (-> any))]))


;; ---- in-amb* (function form) ----

;; Validate the arguments before calling the private `in-amb*/do`:
;; `thk` and `handler` (the empty handler, which answers `(amb)`) must
;; be zero-arity procedures.  If they are not, call the contracted
;; `in-amb*` only to raise its contract error; it never runs the search.
;;
;; `check-args` is a macro, not a function, so that the contracted call
;; is part of the expansion at the use site: the module using `in-amb*`
;; is blamed, not this module.
(define-syntax check-args
  (syntax-rules ()
    [(_ thk) (check-args thk amb-fail)]
    [(_ thk handler)
     (unless (and (procedure? thk)
                  (procedure-arity-includes? thk 0)
                  (procedure? handler)
                  (procedure-arity-includes? handler 0))
       (in-amb* thk handler))]
    [(_ thk #:frontier f)
     (unless (and (procedure? thk)
                  (procedure-arity-includes? thk 0)
                  (or (not f) (amb-frontier? f)))
       (in-amb* thk #:frontier f))]))

;; `*in-amb*` is the public binding for `in-amb*`.
;;
;; As an expression: falls through to the contracted `in-amb*`,
;; returning a lazy stream.
;;
;; Inside a `for` clause: validates the arguments (blaming the
;; caller), then dispatches to `-in-amb*/do` for direct
;; `make-do-sequence` iteration.
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
      [[(id:id ...) (_ expr handler-expr)]
       (syntax/loc stx
         [(id ...)
          (let ([thk expr] [handler handler-expr])
            (check-args thk handler)
            (-in-amb*/do thk handler))])]
      [[(id:id ...) (_ expr #:frontier frontier-expr)]
       (syntax/loc stx
         [(id ...)
          (let ([thk expr] [f frontier-expr])
            (check-args thk #:frontier f)
            (-in-amb*/do thk #:frontier f))])]
      [_ #f])))


;; ---- in-amb (macro form) ----

;; Transformer for the expression position: wraps the body in a thunk
;; and calls the private `in-amb*`.
(define-for-syntax (in-amb stx)
  (syntax-parse stx
    [(_ expr)
     (syntax/loc stx
       (-in-amb* (λ () expr)))]
    [(_ expr #:empty-handler handler)
     (syntax/loc stx
       (in-amb* (λ () expr) handler))]
    [(_ expr #:frontier frontier)
     (syntax/loc stx
       (in-amb* (λ () expr) #:frontier frontier))]))

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
      [[(id:id ...) (_ expr #:empty-handler handler)]
       (syntax/loc stx
         [(id ...)
          (in-amb*/do (λ () expr) handler)])]
      [[(id:id ...) (_ expr #:frontier frontier)]
       (syntax/loc stx
         [(id ...)
          (in-amb*/do (λ () expr) #:frontier frontier)])])))


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
    [(_ expr #:empty-handler handler)
     (syntax/loc stx
       (in-amb*/do (λ () expr) handler))]
    [(_ expr #:frontier frontier)
     (syntax/loc stx
       (in-amb*/do (λ () expr) #:frontier frontier))]))
