#lang racket/base

(require "utils.rkt"
         (for-syntax racket/base syntax/parse)
         racket/case
         racket/sequence
         racket/stream
         goto/unsafe)

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


(define (fail #:empty-handler [empty-handler (current-amb-empty-handler)]
              #:tasks [task* (current-amb-tasks)]
              #:length [length (current-amb-length)])
  (if (zero? (length task*))
      (empty-handler)
      (goto (sequence-ref task* 0))))

(define (amb*₁ alt*)
  (define len (vector-length alt*))
  (define task* (current-amb-tasks))
  ((current-amb-shuffler) alt*)
  (if (zero? len)
      (fail #:tasks task*)
      (let* ([pos #t] [task (label)])
        (case/eq pos
          [(#t)
           (set! pos 0)
           ((current-amb-pusher) task* task)
           (goto (sequence-ref task* 0))]
          [(#f)
           (fail #:tasks task*)])
        (define alt (vector-ref alt* pos))
        (vector-set! alt* pos #f)
        (set! pos (add1 pos))
        (when (= pos len)
          ((current-amb-popper) task*)
          (set! alt* #f)
          (set! pos #f))
        (alt))))

(define (amb* . alt*) (amb*₁ (list->vector alt*)))

(define-syntax (amb stx)
  (syntax-parse stx
    #:datum-literals ()
    [(_ expr ...)
     (syntax/loc stx
       (amb*₁ (vector (λ () expr) ...)))]))


(define (zero) 0)
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
           (amb*₁ (#,derived-stx #:length n #:fill zero (clauses ...) break ... (λ () body ...))))]
        [(_ (clauses ...) break:break-clause ... body ...+)
         (quasisyntax/loc stx
           (amb*₁ (#,derived-stx (clauses ...) break ... (λ () body ...))))]))
    (values (make-for/amb #'for/vector)
            (make-for/amb #'for*/vector))))


(define (in-amb* alt)
  (define break #f)
  (define return #f)
  (define (empty-handler) (break #t))
  (define task* ((current-amb-maker)))
  (define length (current-amb-length))
  (parameterize ([current-amb-empty-handler empty-handler]
                 [current-amb-tasks task*])
    (define task (label))
    (cond
      [(not return)
       ((current-amb-pusher) task* task)
       (for/stream ([_ (in-naturals)])
         #:break
         (let/cc k (set! break k) #f)
         (let/cc k (set! return k)
           (fail #:empty-handler empty-handler
                 #:tasks task*
                 #:length length)))]
      [else
       ((current-amb-popper) task*)
       (call-with-values alt (λ v* (apply return v*)))])))

(define (in-amb*₁ alt)
  (define continue #f)
  (define return #f)
  (define (empty-handler) (continue #f))
  (define task* ((current-amb-maker)))
  (define length (current-amb-length))
  (parameterize ([current-amb-empty-handler empty-handler]
                 [current-amb-tasks task*])
    (define task (label))
    (cond
      [(not return)
       ((current-amb-pusher) task* task)
       (make-do-sequence
        (λ ()
          (initiate-sequence
           #:init-pos 0
           #:next-pos add1
           #:continue-with-pos?
           (λ (_)
             (let/cc k (set! continue k) #t))
           #:pos->element
           (λ (_)
             (let/cc k (set! return k)
               (fail #:empty-handler empty-handler
                     #:tasks task*
                     #:length length))))))]
      [else
       ((current-amb-popper) task*)
       (call-with-values alt (λ v* (apply return v*)))])))

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
