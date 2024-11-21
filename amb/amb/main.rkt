#lang racket/base

(require "private/utils.rkt"
         (for-syntax racket/base syntax/parse)
         racket/contract
         racket/sequence
         racket/unsafe/undefined)

(provide amb amb*
         for/amb for*/amb
         (rename-out [-in-amb in-amb] [-in-amb* in-amb*])
         (struct-out exn:fail:contract:amb)
         raise-amb-error
         current-amb-empty-handler
         current-amb-shuffler
         current-amb-queue
         current-amb-queue?
         current-amb-queue-length
         current-amb-queue-empty?
         current-amb-make-queue
         current-amb-enqueue!
         current-amb-dequeue!
         schedule-amb-tasks!)


(define (amb* alt*)
  (let/cc k
    (schedule-amb-tasks! alt* k)
    (if ((current-amb-queue-empty?)
         (current-amb-queue))
        ((current-amb-empty-handler))
        (((current-amb-dequeue!)
          (current-amb-queue))))))

(define-syntax (amb stx)
  (syntax-parse stx
    #:datum-literals ()
    [(_ expr ...)
     (syntax/loc stx (amb* (list (λ () expr) ...)))]))


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


(define (in-amb* thk)
  (define queue?       (current-amb-queue?))
  (define queue-length (current-amb-queue-length))
  (define queue-empty? (current-amb-queue-empty?))
  (define make-queue   (current-amb-make-queue))
  (define enqueue!     (current-amb-enqueue!))
  (define dequeue!     (current-amb-dequeue!))
  (make-do-sequence
   (λ ()
     (define queue (make-queue))
     (define continue unsafe-undefined)
     (define return   unsafe-undefined)
     (define (break) (continue #f))
     (define (call . v*) (apply return v*))
     (define (task) (call-with-values thk call))
     (initiate-sequence
      #:init-pos (enqueue! queue task)
      #:next-pos void
      #:continue-with-pos?
      (λ (_) (let/cc k (set! continue k) #t))
      #:pos->element
      (λ (_)
        (let/cc k
          (set! return k)
          (parameterize ([current-amb-queue         queue]
                         [current-amb-queue?        queue?]
                         [current-amb-queue-length  queue-length]
                         [current-amb-queue-empty?  queue-empty?]
                         [current-amb-make-queue    make-queue]
                         [current-amb-enqueue!      enqueue!]
                         [current-amb-dequeue!      dequeue!]
                         [current-amb-empty-handler break])
            (amb))))))))

(define-for-syntax (in-amb*-parser stx)
  (syntax-parse stx
    #:datum-literals ()
    [[(id:id ...) (_ thk)]
     (syntax/loc stx
       [(id ...)
        (:do-in
         ([(queue?)       (current-amb-queue?)]
          [(queue-length) (current-amb-queue-length)]
          [(queue-empty?) (current-amb-queue-empty?)]
          [(make-queue)   (current-amb-make-queue)]
          [(enqueue!)     (current-amb-enqueue!)]
          [(dequeue!)     (current-amb-dequeue!)])
         (begin
           (define queue (make-queue))
           (define continue unsafe-undefined)
           (define return   unsafe-undefined)
           (define (break) (continue #f))
           (define (call . v*) (apply return v*))
           (define (task) (call-with-values thk call))
           (enqueue! queue task))
         ()
         (let/cc k (set! continue k) #t)
         ([(id ...)
           (let/cc k
             (set! return k)
             (parameterize ([current-amb-queue         queue]
                            [current-amb-queue?        queue?]
                            [current-amb-queue-length  queue-length]
                            [current-amb-queue-empty?  queue-empty?]
                            [current-amb-make-queue    make-queue]
                            [current-amb-enqueue!      enqueue!]
                            [current-amb-dequeue!      dequeue!]
                            [current-amb-empty-handler break])
               (amb)))])
         #t
         #t
         ())])]))

(define-sequence-syntax -in-amb* (λ () #'in-amb*) in-amb*-parser)


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

(define-sequence-syntax -in-amb in-amb in-amb-parser)
