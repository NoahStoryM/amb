#lang racket/base

;; Core implementation of the ambiguous operator.  This module
;; defines the runtime primitives used by the public API in
;; "amb/main.rkt" and its typed counterpart.

(require "utils.rkt"
         (for-syntax racket/base syntax/parse)
         racket/case
         racket/sequence
         racket/stream
         goto/no-check)

(provide amb amb* unsafe-amb*
         for/amb for*/amb
         in-amb  in-amb*
         in-amb/do in-amb*/do
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
  ;; Jump to the next pending task or trigger the empty handler when
  ;; no choices remain.
  (if (zero? (length task*))
      (empty-handler)
      (goto (sequence-ref task* 0))))

(define (unsafe-amb* alt*)
  ;; Process a vector of thunks sequentially, using the task queue to
  ;; backtrack whenever a thunk signals failure via `amb`.
  (define len (vector-length alt*))
  (define task* (current-amb-tasks))
  ((current-amb-shuffler) alt*)         ; allow user provided randomization
  (if (zero? len)
      (fail #:tasks task*)
      (let* ([pos #t] [task (label)])
        (case/eqv pos
          [(#t)
           ;; first entry
           (set! pos 0)
           ((current-amb-pusher) task* task)
           (goto (sequence-ref task* 0))]
          [(#f)
           ;; no more alternatives
           (fail #:tasks task*)])
        (define alt (vector-ref alt* pos))
        (vector-set! alt* pos #f)
        (set! pos (add1 pos))
        (when (= pos len)
          ((current-amb-popper) task*)
          (set! alt* #f)
          (set! pos #f))
        (alt))))

(define (amb* . alt*)
  ;; Public-facing helper that accepts any number of thunks and
  ;; delegates to `unsafe-amb*` after packing them into a vector.
  (unsafe-amb* (list->vector alt*)))

(define-syntax (amb stx)
  ;; Macro form for writing `(amb expr ...)`.  Each expression is
  ;; delayed, and the resulting alternatives are attempted in an
  ;; unspecified order.
  (syntax-parse stx
    #:datum-literals ()
    [(_ expr ...)
     (syntax/loc stx
       (unsafe-amb* (vector (λ () expr) ...)))]))


(define (zero) 0) ; helper used as the default #:fill value
(define-syntaxes (for/amb for*/amb)
  ;; Variants of `for` that evaluate the body in an ambiguous
  ;; context.  Each clause behaves like its `for/vector` equivalent
  ;; but backtracks on `(amb)`.
  (let ()
    (define-splicing-syntax-class break-clause
      [pattern (~seq (~or* #:break #:final) guard:expr)])
    (define ((make-for/amb derived-stx) stx)
      (syntax-parse stx
        #:datum-literals ()
        [(_ #:length n #:fill fill (clauses ...) break:break-clause ... body ...+)
         (quasisyntax/loc stx
           (unsafe-amb* (#,derived-stx #:length n #:fill (λ () fill) (clauses ...) break ... (λ () body ...))))]
        [(_ #:length n (clauses ...) break:break-clause ... body ...+)
         (quasisyntax/loc stx
           (unsafe-amb* (#,derived-stx #:length n #:fill zero (clauses ...) break ... (λ () body ...))))]
        [(_ (clauses ...) break:break-clause ... body ...+)
         (quasisyntax/loc stx
           (unsafe-amb* (#,derived-stx (clauses ...) break ... (λ () body ...))))]))
    (values (make-for/amb #'for/vector)
            (make-for/amb #'for*/vector))))


(define (in-amb* alt)
  ;; Convert a thunk producing multiple values into a sequence by
  ;; repeatedly invoking it while maintaining a task queue.
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

(define (in-amb*/do alt)
  ;; Version of `in-amb*` that returns a do-sequence for use in
  ;; sequence comprehensions.
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

(define-syntaxes (in-amb in-amb/do)
  ;; Macros expanding to calls to `in-amb*` or `in-amb*/do` with the
  ;; expression wrapped in a thunk.  They provide convenient sequence
  ;; syntax for iterating over ambiguous computations.
  (let ()
    (define ((make derived-stx) stx)
      (syntax-parse stx
        #:datum-literals ()
        [(_ expr)
         (quasisyntax/loc stx
           (#,derived-stx (λ () expr)))]))
    (values (make #'in-amb*)
            (make #'in-amb*/do))))
