#lang racket/base

(require (for-syntax racket/base syntax/parse)
         racket/contract/base
         racket/mutability)
(require (rename-in "core.rkt"
                    [in-amb*  -in-amb*]
                    [in-amb*₁ -in-amb*₁])
         (contract-in "core.rkt"
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
          [amb* (-> (-> any) ... any)]
          [raise-amb-error (-> none/c)]
          [current-amb-empty-handler #;(parameter/c (-> none/c)) parameter?]
          [current-amb-shuffler #;(parameter/c (-> mutable-vector? void?)) parameter?]
          [current-amb-maker    #;(parameter/c (-> sequence?)) parameter?]
          [current-amb-tasks    #;(parameter/c sequence?) parameter?]
          [current-amb-length   #;(parameter/c (-> sequence? exact-nonnegative-integer?)) parameter?]
          [current-amb-pusher   #;(parameter/c (-> sequence? amb-task? void?)) parameter?]
          [current-amb-popper   #;(parameter/c (-> sequence? amb-task?)) parameter?]))


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
     (syntax/loc stx
       (-in-amb* (λ () expr)))]))

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
     (syntax/loc stx
       (in-amb*₁ (λ () expr)))]))
