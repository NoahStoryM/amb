#lang racket/base

(require (for-syntax racket/base syntax/parse)
         racket/contract
         racket/mutable-treelist
         racket/unsafe/undefined)
(require (except-in "base.rkt" in-amb in-amb*)
         (rename-in "base.rkt" [in-amb* -in-amb*])
         (contract-in "base.rkt" [in-amb* (-> (-> any) sequence?)]))

(provide amb
         for/amb for*/amb
         (rename-out [*in-amb in-amb] [*in-amb* in-amb*])
         (contract-out
          (struct exn:fail:contract:amb
            ([message string?]
             [continuation-marks continuation-mark-set?]))
          [amb* (-> (listof (-> any)) any)]
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

(define-for-syntax (in-amb*-parser stx)
  (syntax-parse stx
    #:datum-literals ()
    [[(id:id ...) (_ expr)]
     (syntax/loc stx
       [(id ...)
        (:do-in
         ([(thk) expr])
         (begin
           (check-thk thk)
           (define continue unsafe-undefined)
           (define return unsafe-undefined)
           (define (break) (continue #f))
           (define (call . v*) (apply return v*))
           (define (amb-task) (call-with-values thk call))
           (define amb-tasks (mutable-treelist amb-task)))
         ()
         (let/cc k (set! continue k) #t)
         ([(id ...)
           (let/cc k
             (set! return k)
             (parameterize ([current-amb-tasks amb-tasks]
                            [current-amb-empty-handler break])
               (amb)))])
         #t
         #t
         ())])]))

(define-sequence-syntax *in-amb* (λ () #'in-amb*) in-amb*-parser)


(define-for-syntax (in-amb stx)
  (syntax-parse stx
    #:datum-literals ()
    [(_ expr)
     (syntax/loc stx (-in-amb* (λ () expr)))]))

(define-for-syntax (in-amb-parser stx)
  (syntax-parse stx
    #:datum-literals ()
    [[(id:id ...) (_ expr)]
     (in-amb*-parser (syntax/loc stx [(id ...) (_ (λ () expr))]))]))

(define-sequence-syntax *in-amb in-amb in-amb-parser)
