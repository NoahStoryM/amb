#lang racket/base

(require (for-syntax racket/base syntax/parse)
         racket/contract
         racket/unsafe/undefined
         data/queue)
(require (except-in "base.rkt" in-amb in-amb*)
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
          [current-amb-queue    (parameter/c queue?)]
          [current-amb-enqueue! (parameter/c (-> queue? (-> none/c) void?))]
          [current-amb-dequeue! (parameter/c (-> queue? (-> none/c)))]
          [schedule-amb-tasks!  (->* (continuation? (listof (-> any))) (queue?) void?)]))


(define-for-syntax (in-amb*-parser stx)
  (syntax-parse stx
    #:datum-literals ()
    [[(id:id ...) (_ thk)]
     (syntax/loc stx
       [(id ...)
        (:do-in
         ([(amb-queue) (make-queue)]
          [(continue)  unsafe-undefined]
          [(return)    unsafe-undefined])
         (begin
           (define (break) (continue #f))
           (define (call . v*) (apply return v*))
           (define (amb-task) (call-with-values thk call))
           (enqueue! amb-queue amb-task))
         ()
         (let/cc k (set! continue k) #t)
         ([(id ...)
           (let/cc k
             (set! return k)
             (parameterize ([current-amb-queue amb-queue]
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
     (syntax/loc stx (in-amb* (λ () expr)))]))

(define-for-syntax (in-amb-parser stx)
  (syntax-parse stx
    #:datum-literals ()
    [[(id:id ...) (_ expr)]
     (in-amb*-parser (syntax/loc stx [(id ...) (_ (λ () expr))]))]))

(define-sequence-syntax *in-amb in-amb in-amb-parser)
