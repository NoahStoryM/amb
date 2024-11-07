#lang racket/base

(require "private/utils.rkt"
         (for-syntax racket/base syntax/parse)
         racket/contract
         racket/promise
         racket/sequence
         racket/unsafe/undefined
         data/queue)

(provide amb amb* for/amb for*/amb
         (rename-out [in-amb-clause       in-amb]
                     [in-amb/thunk-clause in-amb/thunk])
         (contract-out
          (struct exn:fail:contract:amb
            ([message string?]
             [continuation-marks continuation-mark-set?]))
          #;[in-amb/thunk (-> (-> any) sequence?)]
          [raise-amb-error (-> none/c)]
          [current-amb-empty-handler (parameter/c (-> none/c))]
          [current-amb-shuffler (parameter/c (-> list? list?))]
          [current-amb-queue    (parameter/c queue?)]
          [current-amb-enqueue! (parameter/c (-> queue? (-> none/c) void?))]
          [current-amb-dequeue! (parameter/c (-> queue? (-> none/c)))]
          [schedule-amb-tasks!  (-> continuation? (listof (-> any)) void?)]))


(define-syntax (amb stx)
  (syntax-parse stx
    #:datum-literals ()
    [(_) (syntax/loc stx (amb* ((current-amb-empty-handler))))]
    [(_ alt ...+)
     (syntax/loc stx
       (let/cc k
         (define alt* (list (λ () alt) ...))
         (schedule-amb-tasks! k alt*)
         (((current-amb-dequeue!)
           (current-amb-queue)))))]))

(define-syntax (amb* stx)
  (syntax-parse stx
    #:datum-literals ()
    [(_ expr ...)
     (syntax/loc stx
       (if (non-empty-queue? (current-amb-queue))
           (((current-amb-dequeue!)
             (current-amb-queue)))
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


(define (->false _) #f)
(define (in-amb/thunk thk)
  (define amb-queue (make-queue))
  (define return   unsafe-undefined)
  (define continue unsafe-undefined)
  (define (break) (continue #f))
  (define (call . v*) (apply return v*))
  (make-do-sequence
   (λ ()
     (initiate-sequence
      #:init-pos #t
      #:continue-with-pos?
      (λ (_) (let/cc k (set! continue k) #t))
      #:pos->element
      (λ (pos)
        (let/cc k
          (set! return k)
          (parameterize ([current-amb-queue amb-queue]
                         [current-amb-empty-handler break])
            (if pos (call-with-values thk call) (amb)))))
      #:next-pos ->false))))

(define-for-syntax (in-amb/thunk-parser stx)
  (syntax-parse stx
    #:datum-literals ()
    [[(id:id ...) (_ thk)]
     (syntax/loc stx
       [(id ...)
        (:do-in
         ([(amb-queue) (make-queue)]
          [(return)   unsafe-undefined]
          [(continue) unsafe-undefined])
         (begin
           (define (break) (continue #f))
           (define (call . v*) (apply return v*)))
         ([pos #t])
         (let/cc k (set! continue k) #t)
         ([(id ...)
           (let/cc k
             (set! return k)
             (parameterize ([current-amb-queue amb-queue]
                            [current-amb-empty-handler break])
               (if pos (call-with-values thk call) (amb))))])
         #t
         #t
         (#f))])]))

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
