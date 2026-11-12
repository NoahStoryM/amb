#lang racket/base

;; The kernel implements its specification, and the specification
;; contains the book's search as the special case "every conjunction
;; searches its first goal locally".

(require racket/dict
         racket/match
         rackunit
         "../kernel.rkt"
         "../model.rkt")

(define merges
  (list (cons 'append amb-append)
        (cons 'interleave amb-interleave)
        (cons 'alternate amb-alternate)
        (cons 'level amb-level)))

(define all-forms '(seq alt seq alt local once ifte))

;; Wraps the first goal of every conjunction in `local`.
(define (localize g)
  (match g
    [`(seq ,g1 ,g2) `(seq (local ,(localize g1)) ,(localize g2))]
    [(list form g* ...) (cons form (map localize g*))]
    [_ g]))

(define trials 3000)
(random-seed 20261112)

(for ([(name merge) (in-dict merges)])
  (test-case (format "direct = spec (~a)" name)
    (for ([_ trials])
      (define g (random-goal (add1 (random 12)) all-forms))
      (check-equal? ((direct merge) g) ((spec merge) g) (format "~s" g))))
  (test-case (format "direct, every conjunction local = book (~a)" name)
    (for ([_ trials])
      (define g (random-goal (add1 (random 12)) all-forms))
      (check-equal? ((direct merge) (localize g)) ((book merge) g) (format "~s" g)))))

(test-case "book (interleave) = The Reasoned Schemer, 2nd ed."
  (for ([_ trials])
    (define g (random-goal (add1 (random 12)) all-forms))
    (check-equal? ((book amb-interleave) g) (trs2 g) (format "~s" g))))
