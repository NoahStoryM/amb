#lang racket/base

;; ===================================================================
;; amb: the choice operators, local search, and observing a stream
;; ===================================================================
;;
;; Everything here is defined from the operations of amb/base:
;;
;;   amb, amb*, amb-from     a choice, with the current strategy; amb*
;;                           shuffles its vector first
;;   for/amb, for*/amb       a choice among the iterations of a loop
;;   amb-reflect, ...        a stream back into the computation: a local
;;                           search is a value
;;   in-amb, ...             observing a stream
;;
;; The default strategy is amb-append (depth-first), from amb/merge,
;; whose other merges are provided too.  Strategies that see all the
;; alternatives of a choice are in amb/nary.

(require (for-syntax racket/base syntax/parse)
         racket/stream
         "base.rkt"
         "merge.rkt"
         "private/data/stream.rkt")

(provide
 ;; effects
 amb amb* amb-from for/amb for*/amb amb-fail amb-pause
 current-amb-merge current-amb-shuffler
 ;; strategies that see all the alternatives of a choice
 amb-strategy amb-strategy? amb-strategy-proc
 ;; a local search, as a value, and back
 amb-reify amb-reflect amb-await amb-local amb-once
 ;; strategies
 amb-append amb-interleave amb-alternate amb-level
 ;; observing a stream
 in-amb in-answers amb-take amb-events)


;; ---- Choices ----

;; `current-amb-shuffler` is called on the vector of a choice first,
;; where the choice is made, to put the alternatives in another order.
(define current-amb-shuffler (make-parameter void))

(define (amb* m alts)
  (when (> (vector-length alts) 1)
    ((current-amb-shuffler) alts))
  (amb-choose m alts))

;; An element of the sequence: a choice among all of them, which are
;; generated as the search goes on (see `amb-choose-from`).
(define (amb-from seq [m (current-amb-merge)])
  (amb-choose-from m seq))

;; The strategy belongs to the choice point: `amb` reads it when the
;; choice is made.
(define current-amb-merge (make-parameter amb-append))

(define-syntax amb
  (syntax-rules ()
    [(_) (amb-fail)]
    [(_ e ...) (amb* (current-amb-merge) (vector (λ () e) ...))]))

;; (for/amb (clause ...) body ...) is a choice among the values of the
;; body, one alternative for each iteration; the body runs where the
;; choice was made.
;;
;;   with #:length n   the alternatives are collected in a vector first,
;;                     as by for/vector (at most n of them), and the
;;                     choice is that of `amb*`
;;   without           the iterations go on as the search does, and the
;;                     choice is that of `amb-from`
(define-syntaxes (for/amb for*/amb)
  (let ()
    (define-splicing-syntax-class break
      [pattern (~seq (~or* #:break #:final) guard:expr)])
    (define ((expander for/vector for/stream) stx)
      (syntax-parse stx
        [(_ #:length n (clause ...) b:break ... body ...+)
         #:with (b* ...) #'((~@ . b) ...)
         (quasisyntax/loc stx
           (amb* (current-amb-merge)
                 (trim (#,for/vector #:length n #:fill #f (clause ...) b* ...
                        (λ () body ...)))))]
        [(_ (clause ...) b:break ... body ...+)
         #:with (b* ...) #'((~@ . b) ...)
         (quasisyntax/loc stx
           ((amb-from (#,for/stream (clause ...) b* ... (λ () body ...)))))]))
    (values (expander #'for/vector #'for/stream)
            (expander #'for*/vector #'for*/stream))))

;; The vector without the slots at its end that were not filled.
(define (trim alts)
  (let loop ([n (vector-length alts)])
    (cond
      [(and (positive? n) (not (vector-ref alts (sub1 n)))) (loop (sub1 n))]
      [(= n (vector-length alts)) alts]
      [else
       (let ([v (make-vector n #f)])
         (vector-copy! v 0 alts 0 n)
         v)])))


;; ---- From a stream back to the computation ----

;; Passes the pauses at the front of `s` on to the enclosing search, and
;; returns the rest: '() or a stream that starts with an answer.
(define (amb-await s)
  (cond
    [(procedure? s) (amb-pause) (amb-await (s))]
    [else s]))

;; Returns an answer of `s`, as a choice of the enclosing search among
;; all of them; the pauses of `s` are pauses of the enclosing search.
;; This is the `bind` of the stream: each answer of `s` is followed by
;; the rest of the computation.
(define (amb-reflect s [merge (current-amb-merge)])
  (let ([s (amb-await s)])
    (cond
      [(null? s) (amb-fail)]
      [(eq? (cdr s) done) (apply values (car s))]
      [else
       (amb-choose merge
                   (vector (λ () (apply values (car s)))
                           (λ () (amb-reflect ((cdr s)) merge))))])))

;; (amb-local e) has the answers of e, searched for on their own: the
;; choices of e are merged with each other before the rest of the
;; computation sees any of them.
(define-syntax-rule (amb-local e)
  (amb-reflect (amb-reify (λ () e))))

;; The first answer of (thk), or failure.
(define (amb-once thk)
  (let ([s (amb-await (amb-reify thk))])
    (if (null? s)
        (amb-fail)
        (apply values (car s)))))


;; ---- Observing a stream ----

;; The answers of (e), searched for on their own; see also `in-answers`,
;; `amb-take` and `amb-events` in private/data/stream.rkt.
(define-syntax-rule (in-amb e)
  (in-answers (amb-reify (λ () e))))
