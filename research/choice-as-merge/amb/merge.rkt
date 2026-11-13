#lang racket/base

;; ===================================================================
;; amb/merge: strategies of a choice between two alternatives
;; ===================================================================
;;
;; A merge is a procedure of two arguments, which do not play the same
;; part:
;;
;;   (merge s t)   s   the step stream of the alternative that has
;;                     started: a merge looks at s first
;;                 t   the suspension of the alternative that has not
;;                     started yet: forced only when the merge turns to
;;                     it, so that no alternative runs before its turn
;;
;; A merge sees nothing but step streams (private/data/step-stream.rkt),
;; and only through their operations: this module needs no operation of
;; amb/base.  A step of a stream is what `amb-pause` puts in it.

(require "private/data/catenable.rkt"
         "private/data/step-stream.rkt")

(provide amb-append amb-interleave amb-alternate amb-level)


;; ---- Strategies ----
;;
;; The strategies agree on answers that are ready and on a stream that
;; has ended,
;;
;;   '()      ∨ t  =  t
;;   (a : s)  ∨ t  =  a : (s ∨ t)        (amb-alternate: a : (t ∨ s))
;;
;; and differ in one equation, which says what a step (a pause) of s
;; means:
;;
;;   amb-append       step s ∨ t       =  step (s ∨ t)     s keeps the turn
;;   amb-interleave   step s ∨ t       =  step (t ∨ s)     the turn passes on
;;   amb-level        step s ∨ step t  =  step (s ∨ t)     both wait together
;;
;; A step and a lazy rest are suspensions, and so is t: to pass the turn
;; on is to exchange the two arguments.
;;
;; These equations are the definitions; model.rkt has all four as code.
;; amb-interleave and amb-alternate below are their equations.
;; amb-append and amb-level are associative, and are written to use it
;; (see "Associative strategies").

;; The interleaving of miniKanren (microKanren's `mplus`, the second
;; edition's `append-inf`): the turn passes on at each pause.
(define (amb-interleave s t)
  (step-stream-case s
    [() (step-stream-force t)]
    [(answer vs rest)
     (step-stream-cons vs (step-stream-delay
                           (amb-interleave (step-stream-force rest) t)))]
    [(step next)
     (step-stream-delay (amb-interleave (step-stream-force t) next))]))

;; The turn passes on at each event, an answer too (the first edition's
;; `mplusi`, which has no pauses).
(define (amb-alternate s t)
  (step-stream-case s
    [() (step-stream-force t)]
    [(answer vs rest)
     (step-stream-cons vs (step-stream-delay
                           (amb-alternate (step-stream-force t) rest)))]
    [(step next)
     (step-stream-delay (amb-alternate (step-stream-force t) next))]))


;; ---- Associative strategies ----
;;
;; Written from its equations, a merge is a closure around s, and an event
;; of s passes through one closure for every choice it is under: n nested
;; choices cost n steps for each answer.  For an associative merge,
;;
;;   (s ∨ t) ∨ u  =  s ∨ (t ∨ u)
;;
;; the grouping does not matter, so a stream that is not finished can be a
;; record of what is left to do.  When the thunk that a record is to run
;; next is a record of the same strategy, the two are one record: what is
;; left of the inner one, and then what is left of the outer one.
;;
;; A record is a suspension, like every stream that waits, so a strategy
;; that looks at its arguments only by their events (all of them do)
;; needs to know nothing about it.
;;
;; The equation says that the streams are the same.  That a record runs
;; the thunks it was given when the equations do, and no sooner, is a
;; second property, and how many steps it takes is a third; see README.
;;
;; What is left to do is kept in lists that are appended in constant time
;; (catenable.rkt), because either of the two lists can be the long one.

;; Depth-first search: all of s, then t.
;;
;;   (appended s ts)   the stream (s), then the streams of the thunks ts
(struct appended (s ts)
  #:property prop:procedure
  (λ (self) (append-run (appended-s self) (appended-ts self))))

(define (amb-append s t)
  (step-stream-case s
    [() (step-stream-force t)]
    [(answer vs rest) (step-stream-cons vs (appended rest (cat-of t)))]
    [(step next) (appended next (cat-of t))]))

(define (append-run s ts)
  (if (appended? s)
      (append-run (appended-s s) (cat-append (appended-ts s) ts))
      (let loop ([s (step-stream-force s)] [ts ts])
        (step-stream-case s
          [()
           (if (null? ts)
               empty-step-stream
               (loop (step-stream-force (cat-first ts)) (cat-rest ts)))]
          [(answer vs rest) (step-stream-cons vs (appended rest ts))]
          [(step next) (appended next ts)]))))

;; Breadth-first search by pauses: when s pauses, t runs up to its own
;; pause, and then the merged stream pauses once.  The merged stream is a
;; sequence of rounds; in a round, each stream runs up to its pause.
;;
;;   (leveled ws s ts)   the rest of a round: the thunks ws have paused
;;                       in it, (s) is running, and the thunks ts have
;;                       yet to run; then one pause, and the next round,
;;                       in which they run in the same order
(struct leveled (ws s ts)
  #:property prop:procedure
  (λ (self) (level-run (leveled-ws self) (leveled-s self) (leveled-ts self))))

(define (amb-level s t)
  (step-stream-case s
    [() (step-stream-force t)]
    [(answer vs rest) (step-stream-cons vs (leveled '() rest (cat-of t)))]
    [(step next) (level-run (cat-of next) t '())]))

(define (level-run ws s ts)
  (if (leveled? s)
      (level-run (cat-append ws (leveled-ws s))
                 (leveled-s s)
                 (cat-append (leveled-ts s) ts))
      (step-stream-case (step-stream-force s)
        [() (level-next ws ts)]
        [(answer vs rest) (step-stream-cons vs (leveled ws rest ts))]
        [(step next) (level-next (cat-append ws (cat-of next)) ts)])))

;; The next stream of the round; or the end of the round: the pause, and
;; then the streams that have paused.
(define (level-next ws ts)
  (cond
    [(not (null? ts)) (level-run ws (cat-first ts) (cat-rest ts))]
    [(null? ws) empty-step-stream]
    [else
     (let ([s (cat-first ws)] [ws (cat-rest ws)])
       (if (null? ws)
           s
           (leveled '() s ws)))]))
