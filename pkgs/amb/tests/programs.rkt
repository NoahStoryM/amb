#lang racket/base

;; Whole programs, with the equations of spec-merge.rkt and with
;; amb/merge as the mergers of their choices: the same events.

(require rackunit
         racket/match
         "../main.rkt"
         (prefix-in spec: "spec-merge.rkt")
         (prefix-in flat: "../merge.rkt"))

(define (spec-merge m)
  (case m
    [(append) spec:amb-append]
    [(interleave) spec:amb-interleave]
    [(alternate) spec:amb-alternate]
    [(level) spec:amb-level]))
(define (flat-merge m)
  (case m
    [(append) flat:amb-append]
    [(interleave) flat:amb-interleave]
    [(alternate) flat:amb-alternate]
    [(level) flat:amb-level]))

;;   g ::= (tag x) | fail | pause | (seq g g)
;;       | (alt m g g)        a choice by the strategy m
;;       | (each m n g)       for/amb over n iterations of g
;;       | (each/v m n g)     the same, with #:length
;; A state is the list of the tags passed so far.
(define (events merge-of g fuel)
  (define (run g s)
    (match g
      [`(tag ,x) (cons x s)]
      ['fail (amb-fail)]
      ['pause (amb-pause) s]
      [`(seq ,a ,b) (run b (run a s))]
      [`(alt ,m ,a ,b)
       (parameterize ([current-amb-merger (merge-of m)])
         (amb (run a s) (run b s)))]
      [`(each ,m ,n ,a)
       (parameterize ([current-amb-merger (merge-of m)])
         (for/amb ([i n]) (run a (cons i s))))]
      [`(each/v ,m ,n ,a)
       (parameterize ([current-amb-merger (merge-of m)])
         (for/amb #:length n ([i n]) (run a (cons i s))))]))
  (for/list ([x (in-amb/do (run g '()) #:on-pause (λ () 'pause))]
             [_ fuel])
    x))

(define (random-goal size strategies)
  (define n 0)
  (define (strategy) (list-ref strategies (random (length strategies))))
  (let gen ([size size])
    (cond
      [(<= size 1)
       (case (random 6)
         [(0) 'fail]
         [(1 2) 'pause]
         [else (set! n (add1 n)) `(tag ,n)])]
      [else
       (define l (add1 (random (sub1 size))))
       (case (random 7)
         [(0 1) `(seq ,(gen l) ,(gen (- size l)))]
         [(2) `(,(if (zero? (random 2)) 'each 'each/v) ,(strategy) ,(random 4) ,(gen (sub1 size)))]
         ;; mostly nested on the left
         [(3 4) `(alt ,(strategy) ,(gen (sub1 size)) ,(gen 1))]
         [else `(alt ,(strategy) ,(gen l) ,(gen (- size l)))])])))

(test-case "programs: the two flat strategies"
  (for* ([m '(append level)] [size '(2 4 8 14)] [_ 500])
    (define g (random-goal size (list m)))
    (define fuel (+ 1 (random 60)))
    (check-equal? (events flat-merge g fuel) (events spec-merge g fuel) (format "~s" g))))

(test-case "programs: the four strategies mixed"
  (for* ([size '(2 4 8 14 20)] [_ 600])
    (define g (random-goal size '(append interleave alternate level)))
    (define fuel (+ 1 (random 60)))
    (check-equal? (events flat-merge g fuel) (events spec-merge g fuel) (format "~s" g))))

(test-case "nested on the left"
  (define (left n) (if (= n 0) (amb 0) (amb (left (sub1 n)) n)))
  (for ([m '(append level)])
    (parameterize ([current-amb-merger (flat-merge m)])
      (check-equal? (for/list ([x (in-amb/do (left 20000))]) x)
                    (for/list ([i 20001]) i)))))
