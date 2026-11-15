#lang racket/base

;; ===================================================================
;; Public API for the `amb` library
;; ===================================================================
;;
;; amb/base has the effects of a search; this module adds the syntax
;; that makes the alternatives of a choice.

(require (for-syntax racket/base syntax/parse)
         racket/vector
         "base.rkt")

(provide (all-from-out "base.rkt")
         amb for/amb for*/amb
         sequence->amb
         current-amb-shuffler)


;; ---- Choices ----

;; An element of the sequence: a choice among all of them.
(define (sequence->amb seq)
  (unless (sequence? seq)
    (raise-argument-error 'sequence->amb "sequence?" seq))
  (make-amb (λ () (sequence-generate* seq))))

;; Called once on the vector of the alternatives of every `amb` and
;; every `for/amb` with `#:length`, where the choice is made, to put
;; them in another order.  The vector may be empty.
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

;; A choice among the thunks of the mutable vector `thk*`; the chosen
;; thunk is returned, to be called where the choice was made.
(define (choose thk*)
  ((current-amb-shuffler) thk*)
  (sequence->amb thk*))

;; (amb e ...) is a choice among the expressions: none of them is
;; evaluated before it is chosen.  (amb) fails.
(define-syntax (amb stx)
  (syntax-parse stx
    [(_ expr ...)
     (syntax/loc stx ((choose (vector (λ () expr) ...))))]))

;; (for/amb #:length n (clause ...) body ...+) is a choice among the
;; values of the body, one alternative for each iteration; the body runs
;; where the choice was made.  The alternatives are collected first, as
;; by `for/vector`: at most n of them.  With `#:fill`, the slots that
;; are left are alternatives too, each with the value of the fill
;; expression.
(define-syntaxes (for/amb for*/amb)
  (let ()
    (define-splicing-syntax-class break-clause
      [pattern (~seq (~or* #:break #:final) guard:expr)])
    (define ((make-for/amb for/vector) stx)
      (syntax-parse stx
        [(_ #:length n (~optional (~seq #:fill fill-expr))
            (clause ...) break:break-clause ... body ...+)
         #:with fill (if (attribute fill-expr) #'(λ () fill-expr) #'#f)
         #:with (break* ...) #'((~@ . break) ...)
         (quasisyntax/loc stx
           ((choose
             (trim (#,for/vector #:length n #:fill fill
                    (clause ...) break* ... (λ () body ...))))))]))
    (values (make-for/amb #'for/vector)
            (make-for/amb #'for*/vector))))

;; The vector without the slots at its end that were not filled.
(define (trim thk*)
  (define len (vector-length thk*))
  (let loop ([n len])
    (cond
      [(and (positive? n) (not (vector-ref thk* (sub1 n)))) (loop (sub1 n))]
      [(= n len) thk*]
      [else (vector-copy thk* 0 n)])))
