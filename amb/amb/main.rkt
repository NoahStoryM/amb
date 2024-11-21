#lang racket/base

(require "private/utils.rkt"
         (for-syntax racket/base syntax/parse)
         racket/contract
         racket/sequence
         racket/unsafe/undefined
         data/queue)

(provide amb amb*
         for/amb for*/amb
         in-amb in-amb*
         (struct-out exn:fail:contract:amb)
         raise-amb-error
         current-amb-empty-handler
         current-amb-shuffler
         current-amb-queue
         current-amb-enqueue!
         current-amb-dequeue!
         schedule-amb-tasks!)


(define (amb* alt*)
  (if (null? alt*)
      (if (non-empty-queue? (current-amb-queue))
          (((current-amb-dequeue!)
            (current-amb-queue)))
          ((current-amb-empty-handler)))
      (let/cc k
        (schedule-amb-tasks! alt* k)
        (amb))))

(define-syntax (amb stx)
  (syntax-parse stx
    #:datum-literals ()
    [(_ expr ...) (syntax/loc stx (amb* (list (λ () expr) ...)))]))


(define-syntaxes (for/amb for*/amb)
  (let ()
    (define-splicing-syntax-class break-clause
      [pattern (~seq (~or* #:break #:final) guard:expr)])
    (define ((make-for/amb derived-stx) stx)
      (syntax-parse stx
        [(_ (clause ...) break:break-clause ... body ...+)
         (quasisyntax/loc stx
           (amb* (#,derived-stx (clause ...) break ... (λ () body ...))))]))
    (values (make-for/amb #'for/list)
            (make-for/amb #'for*/list))))


(define (amb*->sequence thk)
  (define amb-queue (make-queue))
  (define continue  unsafe-undefined)
  (define return    unsafe-undefined)
  (make-do-sequence
   (λ ()
     (define (break) (continue #f))
     (define (call . v*) (apply return v*))
     (define (init-task) (call-with-values thk call))
     (initiate-sequence
      #:init-pos (enqueue! amb-queue init-task)
      #:next-pos values
      #:continue-with-pos?
      (λ (_) (let/cc k (set! continue k) #t))
      #:pos->element
      (λ (_)
        (let/cc k
          (set! return k)
          (parameterize ([current-amb-queue amb-queue]
                         [current-amb-empty-handler break])
            (amb))))))))

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
           (define (init-task) (call-with-values thk call))
           (enqueue! amb-queue init-task))
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

(define-sequence-syntax in-amb* (λ () #'amb*->sequence) in-amb*-parser)


(define-for-syntax (amb->sequence stx)
  (syntax-parse stx
    #:datum-literals ()
    [(_ expr)
     (syntax/loc stx (in-amb* (λ () expr)))]))

(define-for-syntax (in-amb-parser stx)
  (syntax-parse stx
    #:datum-literals ()
    [[(id:id ...) (_ expr)]
     (in-amb*-parser (syntax/loc stx [(id ...) (_ (λ () expr))]))]))

(define-sequence-syntax in-amb amb->sequence in-amb-parser)
