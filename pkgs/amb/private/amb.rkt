#lang racket/base

;; Core implementation of the ambiguous operator.  This module
;; defines the runtime primitives used by the public API in
;; "amb/main.rkt" and its typed counterpart.

(require "utils.rkt"
         (for-syntax racket/base syntax/parse)
         racket/case
         racket/sequence
         racket/stream
         goto)

(provide amb amb* unsafe-amb*
         for/amb for*/amb
         in-amb  in-amb*
         in-amb/do in-amb*/do
         (struct-out exn:fail:contract:amb)
         raise-amb-error
         current-amb-empty-handler
         current-amb-shuffler
         current-amb-rotator
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
  (when (zero? (length task*))
    (empty-handler))
  (goto (sequence-ref task* 0)))

(define (unsafe-amb* alt*)
  ;; Process a vector of thunks sequentially, using the task queue to
  ;; backtrack whenever a thunk signals failure via `amb`.
  (define len (vector-length alt*))
  (define task* (current-amb-tasks))
  ((current-amb-shuffler) alt*)         ; allow user provided randomization
  (when (zero? len)
    (fail #:tasks task*))
  (define pos #t)
  (define task (label))
  (case/eq pos
    ;; first entry
    [(#t)
     (set! pos 0)
     ((current-amb-pusher) task* task)
     (goto (sequence-ref task* 0))]
    ;; no more alternatives
    [(#f)
     (fail #:tasks task*)]
    [else
     (when (= pos len)
       (set! pos #f)
       ((current-amb-popper) task*)
       (fail #:tasks task*))])
  (define alt (vector-ref alt* pos))
  (vector-set! alt* pos amb*)
  (set! pos (add1 pos))
  (when (= pos len)
    (set! alt* empty-mutable-vector))
  (when (equal? alt amb*)
    (goto task))
  ((current-amb-rotator) task*)
  (alt))

(define amb*
  ;; Public-facing helper that accepts any number of thunks and
  ;; delegates to `unsafe-amb*` after packing them into a vector.
  (case-λ
    [() (unsafe-amb* empty-mutable-vector)]
    [(alt) (unsafe-amb* (vector alt))]
    [(alt1 alt2) (unsafe-amb* (vector alt1 alt2))]
    [(alt1 alt2 alt3) (unsafe-amb* (vector alt1 alt2 alt3))]
    [alt* (unsafe-amb* (list->vector alt*))]))

(define-syntax (amb stx)
  ;; Macro form for writing `(amb expr ...)`.  Each expression is
  ;; delayed, and the resulting alternatives are attempted in an
  ;; unspecified order.
  (syntax-parse stx
    #:datum-literals ()
    [(_)
     (syntax/loc stx
       (unsafe-amb* empty-mutable-vector))]
    [(_ expr ...)
     (syntax/loc stx
       (unsafe-amb* (vector (λ () expr) ...)))]))


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
        [(_ #:length n (~optional (~seq #:fill fill-expr)) (clauses ...) break:break-clause ... body ...+)
         #:with fill (if (attribute fill-expr) #'(λ () fill-expr) #'amb*)
         (quasisyntax/loc stx
           (unsafe-amb* (#,derived-stx #:length n #:fill fill (clauses ...) break ... (λ () body ...))))]
        [(_ (clauses ...) break:break-clause ... body ...+)
         (quasisyntax/loc stx
           (unsafe-amb* (#,derived-stx (clauses ...) break ... (λ () body ...))))]))
    (values (make-for/amb #'for/vector)
            (make-for/amb #'for*/vector))))


(define-values (in-amb* in-amb*/do)
  (let ()
    (define (make break-value make-sequence)
      (define continue-value (not break-value))
      (λ (alt)
        ;; Convert a thunk producing multiple values into a sequence by
        ;; repeatedly invoking it while maintaining a task queue.
        (define break #f)
        (define return #f)
        (define (empty-handler) (break break-value))
        (define task* ((current-amb-maker)))
        (define length (current-amb-length))
        (parameterize ([current-amb-empty-handler empty-handler]
                       [current-amb-tasks task*])
          (define task (label))
          (cond
            [(not return)
             ((current-amb-pusher) task* task)
             (make-sequence
              (λ (k) (set! break k) continue-value)
              (λ (k) (set! return k)
                (fail #:empty-handler empty-handler
                      #:tasks task*
                      #:length length)))]
            [else
             ((current-amb-popper) task*)
             (call-with-values alt (λ v* (apply return v*)))]))))
    (values
     (make #t
       (λ (p1 p2)
         (for/stream ([_ (in-naturals)])
           #:break
           (call/cc p1)
           (call/cc p2))))
     (make #f
       (λ (p1 p2)
         (make-do-sequence
          (λ ()
            (initiate-sequence
             #:init-pos 0
             #:next-pos add1
             #:continue-with-pos? (λ (_) (call/cc p1))
             #:pos->element (λ (_) (call/cc p2))))))))))

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
