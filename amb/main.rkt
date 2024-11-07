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
          [current-amb-shuffler (parameter/c (-> list? list?))]
          [current-amb-queue    (parameter/c queue?)]
          [current-amb-enqueue! (parameter/c (-> queue? (-> none/c) void?))]
          [current-amb-dequeue! (parameter/c (-> queue? (-> none/c)))]
          [schedule-amb-tasks!  (-> continuation? (listof (-> any)) void?)]))


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
  (define element unsafe-undefined)
  (make-do-sequence
   (λ ()
     (initiate-sequence
      #:init-pos #t
      #:continue-with-pos?
      (let ([return unsafe-undefined])
        (λ (pos)
          (let/cc k
            (set! return k)
            (with-handlers ([exn:fail:contract:amb? ->false])
              (parameterize ([current-amb-queue amb-queue])
                (if pos
                    (call-with-values thk (λ v* (set! element v*) (return #t)))
                    (amb* #f)))))))
      #:pos->element (λ (_) (apply values element))
      #:next-pos ->false))))

(define-for-syntax (in-amb/thunk-parser stx)
  (syntax-parse stx
    #:datum-literals ()
    [[(id:id ...) (_ thk)]
     (syntax/loc stx
       [(id ...)
        (:do-in
         ([(amb-queue) (make-queue)]
          [(element) unsafe-undefined]
          [(return)  unsafe-undefined])
         (begin)
         ([pos #t])
         (let/cc k
           (set! return k)
           (with-handlers ([exn:fail:contract:amb? ->false])
             (parameterize ([current-amb-queue amb-queue])
               (if pos
                   (call-with-values thk (λ v* (set! element v*) (return #t)))
                   (amb* #f)))))
         ([(id ...) (apply values element)])
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
