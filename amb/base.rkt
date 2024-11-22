#lang racket/base

(require "private/utils.rkt"
         (for-syntax racket/base syntax/parse)
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
  (define amb-queue (current-amb-queue))
  (let/cc k
    (schedule-amb-tasks! k alt* amb-queue)
    (if (non-empty-queue? amb-queue)
        (((current-amb-dequeue!) amb-queue))
        ((current-amb-empty-handler)))))

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
  (define amb-queue (make-queue))
  (define continue unsafe-undefined)
  (define return unsafe-undefined)
  (define (break) (continue #f))
  (define (call . v*) (apply return v*))
  (define (amb-task) (call-with-values thk call))
  (enqueue! amb-queue amb-task)
  (make-do-sequence
   (λ ()
     (initiate-sequence
      #:init-pos 0
      #:next-pos add1
      #:continue-with-pos?
      (λ (_) (let/cc k (set! continue k) #t))
      #:pos->element
      (λ (_)
        (let/cc k
          (set! return k)
          (parameterize ([current-amb-queue amb-queue]
                         [current-amb-empty-handler break])
            (amb))))))))

(define-syntax (in-amb stx)
  (syntax-parse stx
    #:datum-literals ()
    [(_ expr)
     (syntax/loc stx (in-amb* (λ () expr)))]))
