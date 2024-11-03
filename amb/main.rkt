#lang racket/base

(require "private/utils.rkt"
         (for-syntax racket/base syntax/parse)
         racket/contract
         racket/stream
         data/queue)

(provide amb amb* for/amb for*/amb in-amb
         (contract-out
          (struct exn:fail:contract:amb
            ([message string?]
             [continuation-marks continuation-mark-set?]))
          [current-amb-shuffler (parameter/c (-> list? list?))]
          [current-amb-queue    (parameter/c queue?)]
          [current-amb-enqueue! (parameter/c (-> queue? (->* () (continuation?) none/c) void?))]
          [current-amb-dequeue! (parameter/c (-> queue? (->* () (continuation?) none/c)))]
          [schedule-amb-tasks! (-> continuation? (listof (-> any)) void?)]))


(define-syntax amb
  (syntax-parser
    #:datum-literals ()
    [(_) #'(amb* (raise (exn:fail:contract:amb
                         "amb: empty amb queue;\n expected at least one amb task\n  in: (amb)"
                         (current-continuation-marks))))]
    [(_ alt ...+)
     #'(let/cc k
         (define alt* (list (λ () alt) ...))
         (schedule-amb-tasks! k alt*)
         (amb))]))

(define-syntax amb*
  (syntax-parser
    #:datum-literals ()
    [(_ v ...)
     #'(if (non-empty-queue? (current-amb-queue))
           (((current-amb-dequeue!)
             (current-amb-queue)))
           (values v ...))]))

(define-syntaxes (for/amb for*/amb)
  (let ()
    (define-splicing-syntax-class break-clause
      [pattern (~seq (~or* #:break #:final) guard:expr)])
    (define (make-for/amb derived-stx)
      (syntax-parser
        [(_ (clause ...) break:break-clause ... body ...+)
         #`(let/cc k
             (define alt* (#,derived-stx (clause ...) break ... (λ () body ...)))
             (schedule-amb-tasks! k alt*)
             (amb))]))
    (values (make-for/amb #'for/list)
            (make-for/amb #'for*/list))))

(define-syntax in-amb
  (syntax-parser
    #:datum-literals ()
    [(_ expr)
     #'(in-stream
        (let ([amb-queue (make-queue)]
              [first-pass? #t]
              [thk (λ () expr)])
          (let gen-stream ()
            (if (or first-pass? (non-empty-queue? amb-queue))
                (stream-cons
                 (parameterize ([current-amb-queue amb-queue])
                   (cond
                     [(non-empty-queue? amb-queue)
                      (call/cc ((current-amb-dequeue!) amb-queue))]
                     [else
                      (set! first-pass? #f)
                      (thk)]))
                 (gen-stream))
                empty-stream))))]))
