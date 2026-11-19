#lang racket/base

;; ===================================================================
;; The standard mergers
;; ===================================================================
;;
;; A merger makes one generator of two (see private/generator.rkt for
;; the protocol).  The four mergers here agree on answers and on the end
;; of the first generator,
;;
;;   (a : s) ∨ t  =  a : (s ∨ t)        (amb-alternate: a : (t ∨ s))
;;   end     ∨ t  =  t
;;
;; and differ in what a pause of the first generator means:
;;
;;   amb-append       pause s ∨ t        =  pause (s ∨ t)    s keeps the turn
;;   amb-interleave   pause s ∨ t        =  pause (t ∨ s)    the turn passes on
;;   amb-level        pause s ∨ pause t  =  pause (s ∨ t)    both wait together
;;
;; n pauses are n times one pause.
;;
;; amb-interleave and amb-alternate are written as these equations.
;; amb-append and amb-level are associative, and are written to use it:
;; see "Associative mergers" below.  tests/spec-merge.rkt has all four
;; as equations, and tests/merge.rkt compares them.
;;
;; A merger raises when a generator that it calls returns something that
;; is not a result.  It does not check a generator that goes on by
;; itself (end ∨ t = t): whoever calls that one does.
;;
;; This module does not depend on the effects of amb/base.

(require "private/generator.rkt"
         "private/data/catenable.rkt")

(provide amb-append amb-interleave amb-alternate amb-level)


;; ---- By the equations ----

;; The interleaving of miniKanren: the turn passes on at each pause.
(define ((amb-interleave s t))
  (let-values ([(v s) (s)])
    (cond
      [(list? v) (values v (amb-interleave s t))]
      [(exact-positive-integer? v) (values 1 (amb-interleave t (pauses (sub1 v) s)))]
      [(not v) (t)]
      [else (raise-result-error 'amb-interleave v)])))

;; The turn passes on at each event, an answer too.
(define ((amb-alternate s t))
  (let-values ([(v s) (s)])
    (cond
      [(list? v) (values v (amb-alternate t s))]
      [(exact-positive-integer? v) (values 1 (amb-alternate t (pauses (sub1 v) s)))]
      [(not v) (t)]
      [else (raise-result-error 'amb-alternate v)])))


;; ---- Associative mergers ----
;;
;; Written as its equation, a merger is a closure around its two
;; generators, and an event passes through one closure for every merge
;; it is under.  For an associative merger,
;;
;;   (s ∨ t) ∨ u  =  s ∨ (t ∨ u)
;;
;; the grouping does not matter, so what is left of a merge can be a
;; record of generators instead; and when the generator that a record is
;; to call next is a record of the same merger, the two are one record:
;; it takes the generators of the other in, without calling it.
;;
;; A record is called as a generator, so the other mergers and `make-amb`
;; need to know nothing of it.  A merger takes in only its own records.
;;
;; The records and the equations are the same in two senses:
;;
;;   the same events   n pauses being n times one pause; up to the end,
;;                     or up to the call that does not return;
;;   the same calls    before every event, the same generators have
;;                     been called, in the same order.
;;
;; The raw results need not be the same: a count of pauses may come out
;; in other pieces.
;;
;; The generators of a record are kept in lists that are appended in
;; constant time (private/data/catenable.rkt).

;; Depth-first search: all of s, then t.
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
         ;; n pauses stay together: no turn passes on at a pause
         [(or (list? v) (exact-positive-integer? v))
          (define right (and (eq? s end) (cat-singleton? ts) (cat-first ts)))
          ;; Keep impersonated generators behind this merger so an outer
          ;; merger cannot fuse them and bypass a wrapper's procedure call.
          (values v (if (and (procedure? right) (not (impersonator? right)))
                        right
                        (appended s ts)))]
         [(not v) (append-run (cat-first ts) (cat-rest ts))]
         [else (raise-result-error 'amb-append v)]))]))

;; Breadth-first search by pauses, in rounds: in a round, each generator
;; runs up to its pause; then the merged generator pauses once.
;;
;;   (leveled ws s ts)   the rest of a round: the generators ws have
;;                       paused in it, s is running, and the generators
;;                       ts have yet to run; then one pause, and the next
;;                       round, in which they run in the same order
;;
;; A generator that returns n pauses waits with the other n - 1 of them
;; in front of it: it has one pause for each of the next rounds.
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
         [(exact-positive-integer? v)
          (level-next (cat-append ws (cat-of (pauses (sub1 v) s))) ts)]
         [(not v) (level-next ws ts)]
         [else (raise-result-error 'amb-level v)]))]))

;; The next generator of the round; or the end of the round: one pause,
;; and then those that have paused.
(define (level-next ws ts)
  (cond
    [(not (null? ts)) (level-run ws (cat-first ts) (cat-rest ts))]
    [(null? ws) (end)]
    [else
     (let ([s (cat-first ws)] [ws (cat-rest ws)])
       (values 1 (if (null? ws) s (leveled '() s ws))))]))
