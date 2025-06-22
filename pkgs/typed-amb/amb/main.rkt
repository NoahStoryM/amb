#lang typed/racket/base

;; Typed front-end for `amb`.  Provides the same API as the untyped
;; version but with Typed Racket contracts.

(require "private/amb.rkt"
         (for-syntax racket/base syntax/parse))

(provide amb amb*
         for/amb for*/amb
        (rename-out [*in-amb in-amb] [*in-amb* in-amb*])
        in-amb/do in-amb*/do
         (struct-out exn:fail:contract:amb)
         raise-amb-error
         current-amb-empty-handler
         current-amb-shuffler
         current-amb-maker
         current-amb-tasks
         current-amb-length
         current-amb-pusher
         current-amb-popper)


(define-sequence-syntax *in-amb*
  ;; Typed variant of `*in-amb*` that directly uses the typed core.
  (λ () #'in-amb*)
  (λ (stx)
    (syntax-parse stx
      #:datum-literals ()
      [[(id:id ...) (_ expr)]
       (syntax/loc stx
         [(id ...)
          (in-amb*/do expr)])])))


(define-for-syntax (in-amb stx)
  (syntax-parse stx
    #:datum-literals ()
    [(_ expr)
     (syntax/loc stx
       (in-amb* (λ () expr)))]))

(define-sequence-syntax *in-amb
  ;; Equivalent of `*in-amb` from the base module but working with
  ;; typed functions.
  in-amb
  (λ (stx)
    (syntax-parse stx
      #:datum-literals ()
      [[(id:id ...) (_ expr)]
       (syntax/loc stx
         [(id ...)
          (in-amb*/do (λ () expr))])])))


(define-syntax (in-amb/do stx)
  ;; Short-hand macro that expands to `in-amb*/do` with the given
  ;; expression wrapped in a thunk.
  (syntax-parse stx
    #:datum-literals ()
    [(_ expr)
     (syntax/loc stx
       (in-amb*/do (λ () expr)))]))
