#lang racket/base

;; Runtime support for the choice syntax in amb/main and typed/amb.
;; Keep it here so the typed front end only needs to give it types.

(require "core.rkt")

(provide sequence->amb
         amb-local*
         current-amb-shuffler
         thunks choose choose-none
         make-choice-generator)

;; An element of the sequence: a choice among all of them.
(define (sequence->amb seq)
  (unless (sequence? seq)
    (raise-argument-error 'sequence->amb "sequence?" seq))
  (make-amb (λ () (sequence-generate* seq))))

;; Find the answers of a local search before the enclosing computation
;; continues with them.  Its pauses remain pauses of the enclosing search.
(define (amb-local* thk)
  (unless (and (procedure? thk) (procedure-arity-includes? thk 0))
    (raise-argument-error 'amb-local* "(-> any)" thk))
  (make-amb (λ () (amb-generate* thk))))

;; Called once on each vector of alternatives, even an empty vector.
(define current-amb-shuffler
  (make-parameter
   void
   (λ (shuffle!)
     (unless (and (procedure? shuffle!) (procedure-arity-includes? shuffle! 1))
       (raise-argument-error 'current-amb-shuffler
                             "(-> mutable-vector? void?)"
                             shuffle!))
     shuffle!)
   'current-amb-shuffler))

;; The typed syntax uses this to infer one result type for its thunks.
(define (thunks thk . thk*)
  (list->vector (cons thk thk*)))

;; Return the chosen thunk; the syntax calls it at the choice point.
(define (choose thk*)
  (with-amb-mark ((current-amb-shuffler) thk*))
  (sequence->amb thk*))

;; The typed syntax handles the empty choice separately from `thunks`.
(define (choose-none)
  (with-amb-mark ((current-amb-shuffler) (vector)))
  (amb-fail))

;; Turn a producer into a single-pass generator.  The producer calls
;; yield once for each alternative; each next call resumes it there.
(define (make-choice-generator produce)
  (define tag (make-continuation-prompt-tag 'for/amb))
  (define retry #f)
  (define (yield alt)
    (define choice (call/cc values tag))
    (when choice
      (set! retry choice)
      (abort-current-continuation tag (list alt))))
  (define (next)
    (values
     (call-with-continuation-prompt
      (λ ()
        (when retry (retry #f))
        (produce yield)
        #f)
      tag values)
     next))
  next)
