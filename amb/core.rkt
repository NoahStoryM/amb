#lang racket/base

(require "private/utils.rkt"
         (for-syntax racket/base syntax/parse)
         racket/mutable-treelist
         racket/treelist
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
         current-amb-tasks
         current-amb-pusher
         current-amb-popper)


(define (schedule-amb-tasks! k alt* tasks)
  (define shuffle! (current-amb-shuffler))
  (if (= 0 (vector-length alt*))
      (shuffle! alt*)
      (let ([push! (current-amb-pusher)])
        (define (alt->amb-task alt)
          (define (amb-task) (call-in-continuation k alt))
          amb-task)
        (cond
          [(or (eq? push! mutable-treelist-add!)
               (eq? push! mutable-treelist-cons!))
           (cond
             [(eq? push! mutable-treelist-add!) (shuffle! alt*)]
             [(eq? shuffle! vector-reverse!) (void)]
             #;[(eq? shuffle! vector-shuffle!) (shuffle! alt*)]
             [else (shuffle! alt*) (vector-reverse! alt*)])
           (vector-map! alt->amb-task alt*)
           (define new-tasks (vector->treelist alt*))
           (if (eq? push! mutable-treelist-add!)
               (mutable-treelist-append!  tasks new-tasks)
               (mutable-treelist-prepend! new-tasks tasks))]
          [else
           (shuffle! alt*)
           (for ([alt (in-vector alt*)])
             (push! tasks (alt->amb-task alt)))]))))


(define (amb*₁ alt*)
  (define tasks (current-amb-tasks))
  (let/cc k
    (schedule-amb-tasks! k alt* tasks)
    (if (= 0 (mutable-treelist-length tasks))
        ((current-amb-empty-handler))
        (((current-amb-popper) tasks)))))

(define (amb* . alt*) (amb*₁ (list->vector alt*)))

(define-syntax (amb stx)
  (syntax-parse stx
    #:datum-literals ()
    [(_ expr ...)
     (syntax/loc stx (amb*₁ (vector (λ () expr) ...)))]))


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


(define (in-amb* thk)
  (let/cc return
    (define (sync . v*) (apply return v*))
    (define exit unsafe-undefined)
    (define (amb-task) (call-in-continuation exit thk))
    (define tasks (mutable-treelist amb-task))
    (define break unsafe-undefined)
    (define (empty-handler) (break #t))
    (define ((wrap alt))
      (parameterize ([current-amb-tasks         tasks]
                     [current-amb-empty-handler empty-handler])
        (alt)))
    (define s
      (for/stream ([_ (in-naturals)])
        #:break
        (let/cc k (set! break k) #f)
        (let/cc k (set! return k) ((wrap amb*)))))
    (call-with-values (wrap (λ () (let/cc k (set! exit k) s))) sync)))

(define (in-amb*₁ thk)
  (let/cc return
    (define (sync . v*) (apply return v*))
    (define exit unsafe-undefined)
    (define (amb-task) (call-in-continuation exit thk))
    (define tasks (mutable-treelist amb-task))
    (define continue unsafe-undefined)
    (define (empty-handler) (continue #f))
    (define ((wrap alt))
      (parameterize ([current-amb-tasks         tasks]
                     [current-amb-empty-handler empty-handler])
        (alt)))
    (define s
      (make-do-sequence
       (λ ()
         (initiate-sequence
          #:init-pos 0
          #:next-pos add1
          #:continue-with-pos? (λ (_) (let/cc k (set! continue k) #t))
          #:pos->element       (λ (_) (let/cc k (set! return k) ((wrap amb*))))))))
    (call-with-values (wrap (λ () (let/cc k (set! exit k) s))) sync)))

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
