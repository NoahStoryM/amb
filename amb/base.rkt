#lang racket/base

(require "private/utils.rkt"
         (for-syntax racket/base syntax/parse)
         racket/mutable-treelist
         racket/sequence
         racket/stream
         racket/unsafe/undefined)

(provide amb amb* amb*₁
         for/amb for*/amb
         in-amb  in-amb*
         in-amb₁ in-amb*₁
         (struct-out exn:fail:contract:amb)
         raise-amb-error
         current-amb-empty-handler
         current-amb-shuffler
         current-amb-tasks
         current-amb-pusher
         current-amb-popper
         schedule-amb-tasks!)


(define (amb*₁ alt*)
  (define amb-tasks (current-amb-tasks))
  (let/cc k
    (schedule-amb-tasks! k alt* amb-tasks)
    (if (= (mutable-treelist-length amb-tasks) 0)
        ((current-amb-empty-handler))
        (((current-amb-popper) amb-tasks)))))

(define (amb* . alt*) (amb*₁ alt*))

(define-syntax (amb stx)
  (syntax-parse stx
    #:datum-literals ()
    [(_ expr ...)
     (syntax/loc stx (amb* (λ () expr) ...))]))


(define-syntaxes (for/amb for*/amb)
  (let ()
    (define-splicing-syntax-class break-clause
      [pattern (~seq (~or* #:break #:final) guard:expr)])
    (define ((make-for/amb derived-stx) stx)
      (syntax-parse stx
        [(_ (clause ...) break:break-clause ... body ...+)
         (quasisyntax/loc stx
           (amb*₁ (#,derived-stx (clause ...) break ... (λ () body ...))))]))
    (values (make-for/amb #'for/list)
            (make-for/amb #'for*/list))))


(define (in-amb* thk)
  (define return unsafe-undefined)
  (define (call . v*) (apply return v*))
  (define (amb-task) (call-with-values thk call))
  (define amb-tasks (mutable-treelist amb-task))
  (define break unsafe-undefined)
  (define (amb-empty-handler) (break #t))
  (for/stream ([_ (in-naturals)])
    #:break (let/cc k (set! break k) #f)
    (let/cc k
      (set! return k)
      (parameterize ([current-amb-tasks         amb-tasks]
                     [current-amb-empty-handler amb-empty-handler])
        (amb)))))

(define (in-amb*₁ thk)
  (define return unsafe-undefined)
  (define (call . v*) (apply return v*))
  (define (amb-task) (call-with-values thk call))
  (define amb-tasks (mutable-treelist amb-task))
  (define continue unsafe-undefined)
  (define (amb-empty-handler) (continue #f))
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
          (parameterize ([current-amb-tasks         amb-tasks]
                         [current-amb-empty-handler amb-empty-handler])
            (amb))))))))

(define-syntaxes (in-amb in-amb₁)
  (let ()
    (define ((make derived-stx) stx)
      (syntax-parse stx
        #:datum-literals ()
        [(_ expr)
         (quasisyntax/loc stx
           (#,derived-stx (λ () expr)))]))
    (values (make #'in-amb*)
            (make #'in-amb*₁))))
