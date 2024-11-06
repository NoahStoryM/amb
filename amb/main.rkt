#lang racket/base

(require "private/utils.rkt"
         (for-syntax racket/base syntax/parse)
         racket/contract
         racket/promise
         racket/sequence
         data/queue)

(provide amb amb* for/amb for*/amb
         (rename-out [in-amb-clause       in-amb]
                     [in-amb/thunk-clause in-amb/thunk])
         (contract-out
          (struct exn:fail:contract:amb
            ([message string?]
             [continuation-marks continuation-mark-set?]))
          #;[in-amb/thunk (-> (-> any) sequence?)]
          [current-amb-shuffler (parameter/c (-> list? list?))]
          [current-amb-queue    (parameter/c queue?)]
          [current-amb-enqueue! (parameter/c (-> queue? (->* () (continuation?) none/c) void?))]
          [current-amb-dequeue! (parameter/c (-> queue? (->* () (continuation?) none/c)))]
          [current-amb-call     (parameter/c (-> (->* () (continuation?) none/c) none/c))]
          [schedule-amb-tasks!  (-> continuation? (listof (-> any)) void?)]
          [run-next-amb-task!   (-> none/c)]))


(define-syntax (amb stx)
  (syntax-parse stx
    #:datum-literals ()
    [(_)
     (syntax/loc stx
       (amb*
        (raise (exn:fail:contract:amb
                "amb: empty amb queue;\n expected at least one amb task\n  in: (amb)"
                (current-continuation-marks)))))]
    [(_ alt ...+)
     (syntax/loc stx
       (let/cc k
         (define alt* (list (λ () alt) ...))
         (schedule-amb-tasks! k alt*)
         (run-next-amb-task!)))]))

(define-syntax (amb* stx)
  (syntax-parse stx
    #:datum-literals ()
    [(_ expr ...)
     (syntax/loc stx
       (if (non-empty-queue? (current-amb-queue))
           (run-next-amb-task!)
           (values expr ...)))]))


(define-syntaxes (for/amb for*/amb)
  (let ()
    (define-splicing-syntax-class break-clause
      [pattern (~seq (~or* #:break #:final) guard:expr)])
    (define ((make-for/amb derived-stx) stx)
      (syntax-parse stx
        [(_ (clause ...) break:break-clause ... body ...+)
         (quasisyntax/loc stx
           (let/cc k
             (define alt* (#,derived-stx (clause ...) break ... (λ () body ...)))
             (schedule-amb-tasks! k alt*)
             (amb)))]))
    (values (make-for/amb #'for/list)
            (make-for/amb #'for*/list))))


(define (empty-handler _) #f)
(define (in-amb/thunk thk)
  (define amb-queue (make-queue))
  (enqueue! amb-queue (λ (k) (call-in-continuation k thk)))
  (define (next-pos . _)
    (delay
      (parameterize ([current-amb-queue amb-queue]
                     [current-amb-call  call/cc])
        (amb))))
  (define (continue-with-pos? pos)
    (with-handlers ([exn:fail:contract:amb? empty-handler])
      (force pos)
      #t))
  (make-do-sequence
   (λ ()
     (initiate-sequence
      #:pos->element       force
      #:next-pos           next-pos
      #:init-pos           (next-pos)
      #:continue-with-pos? continue-with-pos?))))

(define-for-syntax (in-amb/thunk-parser stx)
  (syntax-parse stx
    #:datum-literals ()
    [[(id:id ...) (_ thk)]
     (syntax/loc stx
       [(id ...)
        (:do-in
         ([(amb-queue) (make-queue)])
         (begin
           (enqueue! amb-queue (λ (k) (call-in-continuation k thk)))
           (define (next-pos)
             (delay
               (parameterize ([current-amb-queue amb-queue]
                              [current-amb-call  call/cc])
                 (amb)))))
         ([pos (next-pos)])
         (with-handlers ([exn:fail:contract:amb? empty-handler])
           (force pos)
           #t)
         ([(id ...) (force pos)])
         #t
         #t
         ((next-pos)))])]))

(define-sequence-syntax in-amb/thunk-clause (λ () #'in-amb/thunk) in-amb/thunk-parser)


(define-for-syntax (in-amb stx)
  (syntax-parse stx
    #:datum-literals ()
    [(_ expr)
     (syntax/loc stx (in-amb/thunk (λ () expr)))]))

(define-for-syntax (in-amb-parser stx)
  (syntax-parse stx
    #:datum-literals ()
    [[(id:id ...) (_ expr)]
     (in-amb/thunk-parser (syntax/loc stx [(id ...) (_ (λ () expr))]))]))

(define-sequence-syntax in-amb-clause in-amb in-amb-parser)
