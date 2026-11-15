#lang racket/base

;; ===================================================================
;; amb-append and amb-level, flat
;; ===================================================================
;;
;; The four merges of demo.rkt are their equations: a merge is a closure
;; around its two generators, and an event passes through one closure
;; for every merge it is under.  amb-append and amb-level are
;; associative, so what is left of a merge can be a record of generators
;; instead, and a record whose next generator is a record of the same
;; kind takes its generators in, without calling it.
;;
;; A record is a generator (it is called as one), so the other merges
;; and make-amb need to know nothing of it.
;;
;; These are the same merges as those of demo.rkt in two senses, which
;; tests.rkt checks:
;;
;;   the same events          after the zeros are deleted and every n is
;;                            written as n pauses, up to the end, or up
;;                            to the call that does not return;
;;   the same calls           at every event, the same generators have
;;                            been called, in the same order.
;;
;; The raw results are not the same: a zero or a count of pauses may come
;; out in other pieces.
;;
;;   Generator = () -> (values Result Generator)
;;   Result    = list? | #f | exact-nonnegative-integer?

(require "catenable.rkt")

(provide amb-append amb-level)

(define natural? exact-nonnegative-integer?)

(define (raise-empty-sequence-error)
  (raise (exn:fail:contract
          "sequence has no more values"
          (current-continuation-marks))))
(define (end) (values #f raise-empty-sequence-error))

(define (pauses n g) (if (zero? n) g (λ () (values 1 (pauses (sub1 n) g)))))


;; ---- amb-append: all of s, then t ----
;;
;;   (appended s ts)   the generator s, then the generators ts in order
(struct appended (s ts)
  #:property prop:procedure
  (λ (self) (append-run (appended-s self) (appended-ts self))))

(define (amb-append s t)
  (appended s (cat-of t)))

(define (append-run s ts)
  (cond
    [(appended? s)
     (append-run (appended-s s) (cat-append (appended-ts s) ts))]
    ;; nothing comes after s: it goes on by itself
    [(null? ts) (s)]
    [else
     (let-values ([(v s) (s)])
       (cond
         ;; a zero is not an event: s keeps the turn
         [(eqv? v 0) (append-run s ts)]
         ;; n pauses stay together: no turn passes on at a pause
         [(or (list? v) (natural? v)) (values v (appended s ts))]
         [(not v) (append-run (cat-first ts) (cat-rest ts))]
         [else (raise-argument-error 'amb-append "(or/c list? natural? #f)" v)]))]))


;; ---- amb-level: rounds; in a round, each runs up to its pause ----
;;
;;   (leveled ws s ts)   the rest of a round: the generators ws have
;;                       paused in it, s is running, and the generators
;;                       ts have yet to run; then one pause, and the next
;;                       round, in which they run in the same order
;;
;; A generator that returns n pauses waits with the other n - 1 of them
;; in front of it, (pauses (sub1 n) s): it has one pause for each of the
;; next rounds, as in the equation.
(struct leveled (ws s ts)
  #:property prop:procedure
  (λ (self) (level-run (leveled-ws self) (leveled-s self) (leveled-ts self))))

(define (amb-level s t)
  (leveled '() s (cat-of t)))

(define (level-run ws s ts)
  (cond
    [(leveled? s)
     (level-run (cat-append ws (leveled-ws s))
                (leveled-s s)
                (cat-append (leveled-ts s) ts))]
    ;; s is the only one left: it goes on by itself
    [(and (null? ws) (null? ts)) (s)]
    [else
     (let-values ([(v s) (s)])
       (cond
         [(list? v) (values v (leveled ws s ts))]
         ;; a zero is not a pause: s keeps the turn, and does not wait
         [(eqv? v 0) (level-run ws s ts)]
         [(natural? v) (level-next (cat-append ws (cat-of (pauses (sub1 v) s))) ts)]
         [(not v) (level-next ws ts)]
         [else (raise-argument-error 'amb-level "(or/c list? natural? #f)" v)]))]))

;; The next generator of the round; or the end of the round: one pause,
;; and then those that have paused.
(define (level-next ws ts)
  (cond
    [(not (null? ts)) (level-run ws (cat-first ts) (cat-rest ts))]
    [(null? ws) (end)]
    [else
     (let ([s (cat-first ws)] [ws (cat-rest ws)])
       (values 1 (if (null? ws) s (leveled '() s ws))))]))
