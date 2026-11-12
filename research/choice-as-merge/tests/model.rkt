#lang racket/base

;; The kernel implements the specification, and the specification
;; contains the book's search as the special case "every conjunction
;; searches its first goal locally".
;;
;; The specification (`spec`, `book`) merges by the equations of the
;; strategies; the kernel has its own code for them, and amb-append and
;; amb-level there are flat.  The goals of the second half choose the
;; strategy of each disjunction at random, so that the records of the
;; flat strategies meet the closures of the others, and each other.

(require racket/match
         rackunit
         "../model.rkt")

(define forms '(seq alt seq alt local once ifte))
(define mixed '(seq alt* seq alt* local once ifte))

;; Wraps the first goal of every conjunction in `local`.
(define (localize g)
  (match g
    [`(seq ,g1 ,g2) `(seq (local ,(localize g1)) ,(localize g2))]
    [(list form g* ...) (cons form (map localize g*))]
    [_ g]))

(define trials 3000)
(random-seed 20261112)

(for* ([forms (list forms mixed)]
       [m strategies])
  (define which
    (if (eq? forms mixed) "every disjunction by its own strategy" "one strategy"))
  (test-case (format "direct = spec (~a; ~a)" m which)
    (for ([_ trials])
      (define g (random-goal (add1 (random 16)) forms))
      (check-equal? ((direct m) g) ((spec m) g) (format "~s" g))))
  (test-case (format "direct, every conjunction local = book (~a; ~a)" m which)
    (for ([_ trials])
      (define g (random-goal (add1 (random 16)) forms))
      (check-equal? ((direct m) (localize g)) ((book m) g) (format "~s" g)))))

(test-case "book (interleave) = The Reasoned Schemer, 2nd ed."
  (for ([_ trials])
    (define g (random-goal (add1 (random 16)) forms))
    (check-equal? ((book 'interleave) g) (trs2 g) (format "~s" g))))
