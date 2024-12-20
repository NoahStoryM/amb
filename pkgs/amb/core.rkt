#lang racket/base

(require "private/utils.rkt"
         (for-syntax racket/base syntax/parse)
         racket/sequence
         racket/stream
         racket/vector
         racket/unsafe/undefined)

(provide amb amb* amb*₁
         for/amb for*/amb
         in-amb  in-amb*
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


(define (schedule-amb-tasks! k alt* tasks)
  (define push! (current-amb-pusher))
  ((current-amb-shuffler) alt*)
  (for ([alt (in-vector alt*)])
    (define (amb-task) (call-in-continuation k alt))
    (push! tasks amb-task)))


(define (amb*₁ alt*)
  (define tasks (current-amb-tasks))
  (let/cc k
    (schedule-amb-tasks! k alt* tasks)
    (if (= 0 ((current-amb-length) tasks))
        ((current-amb-empty-handler))
        (((current-amb-popper) tasks)))))

(define (amb* . alt*) (amb*₁ (list->vector alt*)))

(define-syntax (amb stx)
  (syntax-parse stx
    #:datum-literals ()
    [(_ expr ...)
     (syntax/loc stx
       (amb*₁ (vector (λ () expr) ...)))]))


(define-syntaxes (for/amb for*/amb)
  (let ()
    (define-splicing-syntax-class break-clause
      [pattern (~seq (~or* #:break #:final) guard:expr)])
    (define ((make-for/amb derived-stx) stx)
      (syntax-parse stx
        #:datum-literals ()
        [(_ #:length n #:fill fill (clauses ...) break:break-clause ... body ...+)
         (quasisyntax/loc stx
           (amb*₁ (#,derived-stx #:length n #:fill (λ () fill) (clauses ...) break ... (λ () body ...))))]
        [(_ #:length n (clauses ...) break:break-clause ... body ...+)
         (quasisyntax/loc stx
           (amb*₁ (#,derived-stx #:length n #:fill (λ () 0) (clauses ...) break ... (λ () body ...))))]
        [(_ (clauses ...) break:break-clause ... body ...+)
         (quasisyntax/loc stx
           (amb*₁ (#,derived-stx (clauses ...) break ... (λ () body ...))))]))
    (values (make-for/amb #'for/vector)
            (make-for/amb #'for*/vector))))


(define (in-amb* alt)
  (let/cc return
    (call-with-values
     (λ ()
       (define break unsafe-undefined)
       (define (empty-handler) (break #t))
       (define tasks ((current-amb-maker)))
       (parameterize ([current-amb-empty-handler empty-handler]
                      [current-amb-tasks tasks])
         (let/cc sync
           #;(define (amb-task) (call-in-continuation sync alt))
           ((current-amb-pusher) tasks #;amb-task alt)
           (for/stream ([_ (in-naturals)])
             #:break
             (let/cc k (set! break k) #f)
             (let/cc k (set! return k)
               (call-in-continuation sync amb*))))))
     (λ v* (apply return v*)))))

(define (in-amb*₁ alt)
  (let/cc return
    (call-with-values
     (λ ()
       (define continue unsafe-undefined)
       (define (empty-handler) (continue #f))
       (define tasks ((current-amb-maker)))
       (parameterize ([current-amb-empty-handler empty-handler]
                      [current-amb-tasks tasks])
         (let/cc sync
           #;(define (amb-task) (call-in-continuation sync alt))
           ((current-amb-pusher) tasks #;amb-task alt)
           (make-do-sequence
            (λ ()
              (initiate-sequence
               #:init-pos 0
               #:next-pos add1
               #:continue-with-pos? (λ (_) (let/cc k (set! continue k) #t))
               #:pos->element       (λ (_) (let/cc k (set! return k)
                                             (call-in-continuation sync amb*)))))))))
     (λ v* (apply return v*)))))

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
