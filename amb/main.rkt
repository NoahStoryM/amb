#lang racket/base

(require (for-syntax racket/base syntax/parse)
         racket/contract
         racket/mutable-treelist
         racket/unsafe/undefined)
(require (rename-in "base.rkt"
                    [in-amb*  -in-amb*]
                    [in-amb*₁ -in-amb*₁])
         (contract-in "base.rkt"
                      [in-amb*  (-> (-> any) sequence?)]
                      [in-amb*₁ (-> (-> any) sequence?)]))

(provide amb
         for/amb for*/amb
         (rename-out [*in-amb in-amb] [*in-amb* in-amb*])
         in-amb₁ in-amb*₁
         (contract-out
          (struct exn:fail:contract:amb
            ([message string?]
             [continuation-marks continuation-mark-set?]))
          [amb*  (-> (-> any) ... any)]
          [amb*₁ (-> (listof (-> any)) any)]
          [raise-amb-error (-> none/c)]
          [current-amb-empty-handler (parameter/c (-> none/c))]
          [current-amb-shuffler (parameter/c (-> list? list?))]
          [current-amb-tasks    (parameter/c mutable-treelist?)]
          [current-amb-pusher   (parameter/c (-> mutable-treelist? (-> none/c) void?))]
          [current-amb-popper   (parameter/c (-> mutable-treelist? (-> none/c)))]
          [schedule-amb-tasks!  (->* (continuation? (listof (-> any))) (mutable-treelist?) void?)]))


(define (check-thk thk)
  (unless (and (procedure? thk) (procedure-arity-includes? thk 0))
    ;; break contract
    (in-amb* thk)))

(define-sequence-syntax *in-amb*
  (λ () #'in-amb*)
  (λ (stx)
    (syntax-parse stx
      #:datum-literals ()
      [[(id:id ...) (_ expr)]
       (syntax/loc stx
         [(id ...)
          (let ([thk expr])
            (check-thk thk)
            (-in-amb*₁ thk))])])))


(define-for-syntax (in-amb stx)
  (syntax-parse stx
    #:datum-literals ()
    [(_ expr)
     (syntax/loc stx (-in-amb* (λ () expr)))]))

(define-sequence-syntax *in-amb
  in-amb
  (λ (stx)
    (syntax-parse stx
      #:datum-literals ()
      [[(id:id ...) (_ expr)]
       (syntax/loc stx
         [(id ...)
          (-in-amb*₁ (λ () expr))])])))


(define-syntax (in-amb₁ stx)
  (syntax-parse stx
    #:datum-literals ()
    [(_ expr)
     (syntax/loc stx (in-amb*₁ (λ () expr)))]))
