#lang racket/base

;; The search of ../kanren.rkt, on the programs that ambKanren's
;; research (issue #14) uses to tell searches apart.
;;
;;   direct   disj2 is a choice, conj2 is composition
;;   local    conj2 also searches its first goal locally: the search of
;;            the book (the stream implementation of miniKanren-typed)

(require (except-in rackunit fail)
         "../kernel.rkt"
         "../kanren.rkt")

(define-syntax-rule (direct merge e)
  (parameterize ([current-amb-merge merge]) e))
(define-syntax-rule (local merge e)
  (parameterize ([current-amb-merge merge] [current-conj-local? #t]) e))

(define fair (list amb-interleave amb-alternate amb-level))

(defrel (nevero) (nevero))
(defrel (alwayso) (conde [succeed] [(alwayso)]))
(defrel (anyo g) (conde [g] [(anyo g)]))
(defrel (deepo n)                      ; n pauses, then succeed
  (conde [(== n '())]
         [(fresh (m) (== n (cons 'z m)) (deepo m))]))
(defrel (appendo l t out)
  (conde
    [(== l '()) (== t out)]
    [(fresh (a d res)
       (== (cons a d) l)
       (== (cons a res) out)
       (appendo d t res))]))
(defrel (counto n)                     ; zero, (s zero), (s (s zero)), ...
  (conde [(== n 'zero)]
         [(fresh (m) (== n (list 's m)) (counto m))]))

;; ---- fairness: every fair strategy gives the answers of the book ----

(define-syntax-rule (check-all-fair e expected)
  (for ([merge fair])
    (check-equal? (direct merge e) expected)
    (check-equal? (local merge e) expected)))

(test-case "fairness (C1 to C7 of ambKanren issue #14)"
  ;; C1: the left branch has infinitely many answers
  (check-all-fair (run 4 q (conde [(alwayso) (== q 'L)] [(== q 'R)])) '(R L L L))
  ;; C2: the left branch pauses forever
  (check-all-fair (run 1 q (conde [(nevero)] [(== q 'R)])) '(R))
  (check-all-fair (run 1 q (conde [(nevero)] [(deepo '(z z z)) (== q 'R)])) '(R))
  ;; C3: pauses at different depths
  (check-all-fair (run* q (conde [(deepo '(z z z)) (== q 'deep3)]
                                 [(deepo '(z)) (== q 'deep1)]
                                 [(== q 'now)]))
                  '(now deep1 deep3))
  ;; C4: a conjunction after a disjunction whose left branch never answers
  (check-all-fair (run 2 q (conde [(nevero)] [succeed]) (conde [(== q 1)] [(== q 2)])) '(1 2))
  ;; C5: a disjunction of a conjunction that never answers
  (check-all-fair (run 1 q (conde [(nevero) (== q 1)] [(== q 2)])) '(2))
  ;; C6: the first goal has infinitely many answers, and the second
  ;; pauses forever on the first of them
  (check-all-fair (run 1 q (counto q)
                    (conde [(== q 'zero) (nevero)] [(== q '(s (s zero)))]))
                  '((s (s zero))))
  ;; C7: generate and test
  (check-all-fair (run 1 q (counto q) (== q '(s (s (s zero))))) '((s (s (s zero))))))

(test-case "recursive relations"
  (check-all-fair (run* (x y) (appendo x y '(1 2 3)))
                  '((() (1 2 3)) ((1) (2 3)) ((1 2) (3)) ((1 2 3) ())))
  (check-all-fair (run 3 (x y z) (appendo x y z))
                  '((() _0 _0) ((_0) _1 (_0 . _1)) ((_0 _1) _2 (_0 _1 . _2))))
  (check-all-fair (run 5 q (anyo (conde [(== q 1)] [(== q 2)]))) '(1 2 1 2 1)))

;; ---- committed choice ----

(defrel (oneo q) (== q 'A))
(defrel (late-failo) fail)

(test-case "ifte and once pass the pauses of their test on"
  (check-all-fair (run 1 q (conde [(once (nevero))] [(== q 1)])) '(1))
  (check-all-fair (run 1 q (conde [(conda [(nevero) succeed] [succeed succeed])] [(== q 1)])) '(1))
  ;; the pause of the test is a pause of the goal that runs it
  (check-all-fair (run* q (disj (once (oneo q)) (== q 'B))) '(B A))
  ;; the else branch runs only when the test has no answer at all
  (check-all-fair (run* q (ifte (late-failo) succeed (== q 'ok))) '(ok))
  (check-all-fair (run* q (conda [(conde [(== q 1)] [(== q 2)])] [(== q 3)])) '(1 2))
  (check-all-fair (run* q (condu [(conde [(== q 1)] [(== q 2)])] [(== q 3)])) '(1)))

;; ---- a search that keeps the pauses of its test to itself ----

;; The counterexample pause-escape.rkt of ambKanren's research: a goal
;; that runs a plain search of its own, and does not pass its pauses on.
;; A pause cannot leave the search it belongs to, so no answer is lost;
;; the test is one step of the enclosing search.
(defrel (oneo1 x) (== x 1))
(define ((raw-once g) s)
  (define-values (more? get) (sequence-generate (in-amb (g s))))
  (if (more?) (get) (amb-fail)))

(test-case "a pause does not escape its search"
  (check-all-fair (run* q (raw-once (oneo1 q)) (== q 1)) '(1))
  (check-all-fair (run* q (disj (raw-once (oneo1 q)) (== q 2))) '(1 2)))

;; ---- the order of answers: laws ----
;;
;; Astra's example (algebraic-control-flow-math issue #68): k pauses once
;; more for a than for b and c.

(defrel (skip) succeed)
(defrel (k q r)
  (conde
    [(== q 'a) (skip) (== r 'A)]
    [(== q 'b) (== r 'B)]
    [(== q 'c) (== r 'C)]))
(define (x q) (disj (== q 'a) (== q 'b)))
(define (y q) (== q 'c))
(define (p i) (disj (== i 0) (== i 1)))
(define (f i q) (conde [(== i 0) (x q)] [(== i 1) (y q)]))

(define-syntax-rule (distributivity)
  (list (run* r (fresh (q) (disj (x q) (y q)) (k q r)))
        (run* r (disj (fresh (q) (x q) (k q r)) (fresh (q) (y q) (k q r))))))
(define-syntax-rule (associativity)
  (list (run* r (fresh (i q) (fresh () (p i) (f i q)) (k q r)))
        (run* r (fresh (i q) (p i) (fresh () (f i q) (k q r))))))

(test-case "conjunction: associative and distributive, whatever the strategy"
  (check-equal? (direct amb-interleave (distributivity)) '((C B A) (C B A)))
  (check-equal? (direct amb-interleave (associativity))  '((C B A) (C B A)))
  (check-equal? (direct amb-alternate (distributivity))  '((C B A) (C B A)))
  (check-equal? (direct amb-alternate (associativity))   '((C B A) (C B A)))
  ;; the order of ambKanren's global frontier
  (check-equal? (direct amb-level (distributivity))      '((B C A) (B C A)))
  (check-equal? (direct amb-level (associativity))       '((B C A) (B C A))))

(test-case "the book's conjunction is neither"
  (check-equal? (local amb-interleave (distributivity)) '((A B C) (C B A)))
  (check-equal? (local amb-interleave (associativity))  '((A B C) (C B A))))

(test-case "fail is a unit of disjunction"
  (for ([merge fair])
    (check-equal? (direct merge (run* r (fresh (q) (disj fail (x q)) (k q r))))
                  (direct merge (run* r (fresh (q) (x q) (k q r)))))
    (check-equal? (direct merge (run* r (fresh (q) (disj (x q) fail) (k q r))))
                  (direct merge (run* r (fresh (q) (x q) (k q r)))))))

(test-case "disjunction is associative for amb-level only"
  (define-syntax-rule (grouping)
    (list (run 6 q (disj2 (disj2 (anyo (== q 1)) (anyo (== q 2))) (anyo (== q 3))))
          (run 6 q (disj2 (anyo (== q 1)) (disj2 (anyo (== q 2)) (anyo (== q 3)))))))
  ;; the alternative that is not grouped with another gets every other turn
  (check-equal? (direct amb-interleave (grouping)) '((3 1 3 2 3 1) (1 1 2 1 3 1)))
  (check-equal? (direct amb-level (grouping))      '((1 2 3 1 2 3) (1 2 3 1 2 3))))
