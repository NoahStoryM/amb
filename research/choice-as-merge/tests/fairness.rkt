#lang racket/base

;; What a strategy has to do to be fair, and what no strategy can do.
;;
;; Each program has an answer x.  A strategy finds it, or starves: x is
;; not among the first 4000 events of the search.

(require rackunit
         "../kernel.rkt")

(define fuel 4000)

(define (nevero) (amb-pause) (nevero))
(define (count n) (amb n (begin (amb-pause) (count (add1 n)))))

(define programs
  (list
   ;; a branch that pauses forever
   (cons 'pauses-forever
         (λ () (amb (nevero) 'x)))
   ;; a branch that answers forever, pausing between the answers
   (cons 'answers-and-pauses-forever
         (λ () (amb (count 0) 'x)))
   ;; a branch that answers forever and never pauses
   (cons 'answers-forever
         (λ () (amb (amb-from (in-naturals)) 'x)))
   ;; generate and test: one infinite branch, tested one answer at a time
   (cons 'generate-and-test
         (λ () (let ([n (count 0)]) (if (= n 5) 'x (amb-fail)))))
   ;; the test of the first candidate pauses forever
   (cons 'generate-and-test-forever
         (λ () (let ([n (count 0)])
                 (cond [(= n 0) (nevero)]
                       [(= n 3) 'x]
                       [else (amb-fail)]))))
   ;; a local search whose pauses are passed on
   (cons 'once-pauses-forever
         (λ () (amb (amb-once nevero) 'x)))
   ;; x comes after a computation that pauses forever: it is not on any
   ;; branch of the search
   (cons 'after-pauses-forever
         (λ () (begin (nevero) 'x)))))

;;                               append interleave alternate level
(define expected
  '([pauses-forever              #f     #t         #t        #t]
    [answers-and-pauses-forever  #f     #t         #t        #t]
    [answers-forever             #f     #f         #t        #f]
    [generate-and-test           #t     #t         #t        #t]
    [generate-and-test-forever   #f     #t         #t        #t]
    [once-pauses-forever         #f     #t         #t        #t]
    [after-pauses-forever        #f     #f         #f        #f]))

(define strategies (list amb-append amb-interleave amb-alternate amb-level))

(define (finds? merge thk)
  (parameterize ([current-amb-merge merge])
    (and (memq 'x (amb-events (amb-reify thk) fuel)) #t)))

(for ([p programs] [row expected])
  (test-case (symbol->string (car p))
    (check-equal? (for/list ([merge strategies]) (finds? merge (cdr p)))
                  (cdr row))))

;; Fair is not the same as soon.  x is the last of d + 1 alternatives,
;; and the d before it pause forever.  A choice among many is a choice
;; between the first and the rest, so amb-interleave gives the first
;; alternative every other turn, the second every fourth, ...: the
;; pauses x has to wait for double with every alternative before it.
;; For amb-level, they all pause together, and x is found before the
;; first pause.
(define (deep d)
  (if (zero? d) 'x (amb (nevero) (deep (sub1 d)))))

(define (pauses-before-x merge d)
  (parameterize ([current-amb-merge merge])
    (let loop ([s (amb-reify (λ () (deep d)))] [n 0])
      (if (pair? s) n (loop (s) (add1 n))))))

(test-case "how long x waits"
  (check-equal? (for/list ([d (in-range 1 11)]) (pauses-before-x amb-interleave d))
                '(1 3 7 15 31 63 127 255 511 1023))
  (check-equal? (for/list ([d (in-range 1 11)]) (pauses-before-x amb-alternate d))
                '(1 3 7 15 31 63 127 255 511 1023))
  (check-equal? (for/list ([d (in-range 1 11)]) (pauses-before-x amb-level d))
                '(0 0 0 0 0 0 0 0 0 0)))
