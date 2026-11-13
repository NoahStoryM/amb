#lang racket/base

;; ===================================================================
;; Two strategies that see all the alternatives of a choice
;; ===================================================================
;;
;; A choice by a merge is a choice between the first alternative and the
;; choice among the others: the alternatives are not equals.  A strategy
;; made by `amb-strategy` is given all of them, as a procedure `next`:
;; (next) starts the next alternative and returns its stream, or returns
;; #f when there are no more.
;;
;; Both strategies here take the alternatives in one at a time, so there
;; may be infinitely many.  They differ in what the choice shows of the
;; pauses of its alternatives, and so in what an enclosing choice can do:
;;
;;   amb-rotate     The pauses of the choice are the pauses of its
;;                  alternatives.  Between two alternatives it is
;;                  amb-interleave, event for event.  When no
;;                  alternative pauses, the choice does not pause.
;;
;;   amb-dovetail   The choice works in stages, and pauses once after
;;                  each stage as long as there is more to do, whether
;;                  an alternative has paused or not.  It is not one of
;;                  the merges between two alternatives.

(require "kernel.rkt")

(provide amb-rotate amb-dovetail amb-dovetail/unguarded)

;; The alternatives that have started take turns, and a turn ends at a
;; pause, which is a pause of the choice.  When all of them have had a
;; turn, one more alternative starts, and they take turns again, the
;; newest first.
;;
;;   front   the thunks of the alternatives that have yet to take their
;;           turn, in order
;;   back    those that have taken it, the last one first
;;   more?   whether `next` may have more alternatives
(define amb-rotate
  (amb-strategy
   (λ (next)
     (define (run s front back more?)
       (cond
         [(pair? s) (cons (car s) (λ () (run ((cdr s)) front back more?)))]
         [(null? s) (turn front back more?)]
         [else (λ () (turn front (cons s back) more?))]))
     (define (turn front back more?)
       (cond
         [(pair? front) (run ((car front)) (cdr front) back more?)]
         [more?
          (let ([s (next)])
            (if s
                (run s (reverse back) '() #t)
                (turn (reverse back) '() #f)))]
         [(null? back) '()]
         [else (turn (reverse back) '() #f)]))
     (turn '() '() #t))))

;; Stage r: the alternatives that have started run up to their next
;; pause, in the order in which they started; then one more alternative
;; starts and runs up to its pause.  The pauses of a stage are one pause
;; of the choice, after the stage.
;;
;; What if no alternative waits after a stage, and there may be more
;; alternatives?
;;
;;   amb-dovetail             pauses all the same: the choice pauses after
;;                            every stage, unless nothing is left
;;   amb-dovetail/unguarded   goes on with the next stage at once, so
;;                            the choice pauses only when an alternative
;;                            has paused
;;
;;   front   the thunks of the alternatives that have yet to run in this
;;           stage, in order
;;   back    those that wait for the next stage, the last one first
;;   more    'start when this stage has yet to start an alternative,
;;           'started when it has, #f when there are no more
(define (dovetail guarded?)
  (amb-strategy
   (λ (next)
     (define (run s front back more)
       (cond
         [(pair? s) (cons (car s) (λ () (run ((cdr s)) front back more)))]
         [(null? s) (turn front back more)]
         [else (turn front (cons s back) more)]))
     (define (turn front back more)
       (cond
         [(pair? front) (run ((car front)) (cdr front) back more)]
         [(eq? more 'start)
          (let ([s (next)])
            (if s
                (run s '() back 'started)
                (turn '() back #f)))]
         [(pair? back) (λ () (turn (reverse back) '() (and more 'start)))]
         [(not more) '()]
         [guarded? (λ () (turn '() '() 'start))]
         [else (turn '() '() 'start)]))
     (turn '() '() 'start))))

(define amb-dovetail (dovetail #t))
(define amb-dovetail/unguarded (dovetail #f))
