#lang racket/base

;; Public interface for the ambiguous operator.  This module re-exports
;; the primitives from "amb/private/amb.rkt", adds sequence syntax, and
;; contracts for consumers.

(require (for-syntax racket/base syntax/parse)
         racket/contract/base)
(require (rename-in "private/amb.rkt"
                    [in-amb*    -in-amb*]
                    [in-amb*/do -in-amb*/do])
         (contract-in "private/amb.rkt"
                      [in-amb*    (-> (-> any) sequence?)]
                      [in-amb*/do (-> (-> any) sequence?)]))

(provide amb
         for/amb for*/amb
         (rename-out [*in-amb in-amb] [*in-amb* in-amb*])
         in-amb/do in-amb*/do
         (contract-out
          (struct exn:fail:contract:amb
            ([message string?]
             [continuation-marks continuation-mark-set?]))
          [amb* (-> (-> any) ... any)]
          [raise-amb-error (-> none/c)]
          [current-amb-empty-handler #;(parameter/c (-> none/c)) parameter?]
          [current-amb-shuffler #;(parameter/c (-> mutable-vector? void?)) parameter?]
          [current-amb-rotator  #;(parameter/c (-> sequence? void?)) parameter?]
          [current-amb-maker    #;(parameter/c (-> sequence?)) parameter?]
          [current-amb-tasks    #;(parameter/c sequence?) parameter?]
          [current-amb-length   #;(parameter/c (-> sequence? exact-nonnegative-integer?)) parameter?]
          [current-amb-pusher   #;(parameter/c (-> sequence? amb-task? void?)) parameter?]
          [current-amb-popper   #;(parameter/c (-> sequence? amb-task?)) parameter?]))


(define (check-thk thk)
  ;; Ensure `thk` is a zero-argument thunk.  Used to guard the
  ;; contract for `in-amb*`.
  (unless (and (procedure? thk) (procedure-arity-includes? thk 0))
    ;; intentionally violate the contract
    (in-amb* thk)))

(define-sequence-syntax *in-amb*
  ;; Sequence form used by `in-amb*`.  It wraps the provided expression
  ;; in a thunk and verifies its contract.
  (λ () #'in-amb*)
  (λ (stx)
    (syntax-parse stx
      [[(id:id ...) (_ expr)]
       (syntax/loc stx
         [(id ...)
          (let ([thk expr])
            (check-thk thk)
            (-in-amb*/do thk))])])))


(define-for-syntax (in-amb stx)
  (syntax-parse stx
    [(_ expr)
     (syntax/loc stx
       (-in-amb* (λ () expr)))]))

(define-sequence-syntax *in-amb
  ;; Sequence form for `(in-amb expr)` where the expression is wrapped
  ;; in a thunk before passing to `in-amb*/do`.
  in-amb
  (λ (stx)
    (syntax-parse stx
      [[(id:id ...) (_ expr)]
       (syntax/loc stx
         [(id ...)
          (-in-amb*/do (λ () expr))])])))


(define-syntax (in-amb/do stx)
  ;; Like `in-amb` but expands directly to `in-amb*/do` for use in
  ;; contexts that already expect a sequence.
  (syntax-parse stx
    [(_ expr)
     (syntax/loc stx
       (in-amb*/do (λ () expr)))]))
