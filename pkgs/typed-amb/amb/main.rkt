#lang typed/racket/base

(require "core.rkt"
         (for-syntax racket/base syntax/parse))

(provide amb amb*
         for/amb for*/amb
         (rename-out [*in-amb in-amb] [*in-amb* in-amb*])
         in-amb₁ in-amb*₁
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
  (λ () #'in-amb*)
  (λ (stx)
    (syntax-parse stx
      #:datum-literals ()
      [[(id:id ...) (_ expr)]
       (syntax/loc stx
         [(id ...)
          (in-amb*₁ expr)])])))


(define-for-syntax (in-amb stx)
  (syntax-parse stx
    #:datum-literals ()
    [(_ expr)
     (syntax/loc stx
       (in-amb* (λ () expr)))]))

(define-sequence-syntax *in-amb
  in-amb
  (λ (stx)
    (syntax-parse stx
      #:datum-literals ()
      [[(id:id ...) (_ expr)]
       (syntax/loc stx
         [(id ...)
          (in-amb*₁ (λ () expr))])])))


(define-syntax (in-amb₁ stx)
  (syntax-parse stx
    #:datum-literals ()
    [(_ expr)
     (syntax/loc stx
       (in-amb*₁ (λ () expr)))]))
