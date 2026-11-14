#lang racket/base
(require rackunit racket/list racket/sequence "demo.rkt"
         (prefix-in ref: "kernel.rkt"))

(define mergers (list amb-append amb-interleave amb-alternate amb-level))
(define references (list ref:amb-append ref:amb-interleave ref:amb-alternate ref:amb-level))
(define comparisons 0)
(define (same actual expected)
  (set! comparisons (add1 comparisons))
  (check-equal? actual expected))
(define (source es)
  (λ () (if (null? es) (values #f (λ () (error "terminal next called")))
            (values (car es) (source (cdr es))))))
(define (events g fuel)
  (cond
    [(zero? fuel) '()]
    [else
     (define-values (r next) (g))
     (cond
       [(not r) '(end)]
       [(list? r) (cons r (events next (sub1 fuel)))]
       [(eqv? r 0) (events next fuel)]
       [else
        (unless (exact-positive-integer? r) (error "invalid generator result"))
        (define n (min fuel r))
        (append (make-list n 'pause)
                (if (= n fuel) '() (events next (- fuel n))))])]))

(test-case "zero counts disappear without changing four-merger scheduling"
  (define words '(() (0) (0 0) ((a)) (0 (a) 0) (1 0 (b))
                  (0 2 0 (c) 0) (0 (d) 0 1 (e) 0)))
  (for ([merge mergers] [reference references])
    (for* ([left words] [right words] [fuel '(0 1 3 20)])
      (define (erase es) (filter (λ (e) (not (eqv? e 0))) es))
      (same (events (merge (source left) (source right)) fuel)
            (events (reference (source (erase left)) (source (erase right))) fuel)))))

(test-case "make-amb zero steps are neither failure nor observed pauses"
  (define seen 0)
  (same (for/list ([x (in-amb/do (make-amb (source '(0 0 (42) 0)))
                                #:on-pause (λ () (set! seen (add1 seen)) 'P))]) x) '(42))
  (same seen 0)
  (same (for/list ([x (in-amb/do (make-amb (source '(0 0))))]) x) '())
  (same (for/list ([x (in-amb/do (make-amb (source '(0 2 0 (42) 0)))
                                #:on-pause (λ () 'P))]) x) '(P P 42)))

(test-case "the observer also ignores zero returned by a custom merger"
  (define (zero-first s t) (λ () (values 0 (amb-append s t))))
  (parameterize ([current-amb-merger zero-first])
    (same (for/list ([x (in-amb/do (amb 42) #:on-pause (λ () 'P))]) x) '(42))))

(test-case "single-pass cursor continues and stays empty after exhaustion"
  (define s (in-amb/do (amb 1 2 3)))
  (same (for/first ([x s]) x) 1)
  (same (for/list ([x s]) x) '(2 3))
  (same (for/list ([x s]) x) '())
  (same (for/list ([x s]) x) '())
  (define empty (in-amb/do (amb-fail)))
  (same (for/list ([x empty]) x) '())
  (same (for/list ([x empty]) x) '()))

(test-case "parallel and nested iteration, including guard recursion"
  (same (for/list ([x (in-amb/do (for/amb ([i '(1 2)] [j '(a b)]) (list i j)))]) x)
        '((1 a) (2 b)))
  (same (for/list ([x (in-amb/do (for*/amb ([i '(1 2)] [j (in-range i)]) (list i j)))]) x)
        '((1 0) (2 0) (2 1)))
  (same (for/list ([x (in-amb/do (for*/amb ([i '(1 2)] [j (in-range i)])
                                 #:final #f (list i j)))]) x)
        '((1 0) (2 0) (2 1)))
  (same (for/list ([x (in-amb/do (for*/amb ([i '(1 2)] [j (in-range i)])
                                 #:break (and (= i 2) (= j 1)) (list i j)))]) x)
        '((1 0) (2 0)))
  (same (for/list ([x (in-amb/do (for/amb ([i '(1 2)]) (amb i (- i))))]) x)
        '(1 -1 2 -2)))

(test-case "ordinary zero answers and zero/multiple return values remain answers"
  (same (for/list ([vs (sequence-map list (in-amb/do (amb 0 (values) (values 1 2))))]) vs)
        '((0) () (1 2)))
  (same (for/list ([vs (sequence-map list
                                    (in-amb/do (begin (amb-pause) (values 3 4))
                                               #:on-pause (λ () (values))))]) vs)
        '(() (3 4))))

(test-case "creation is lazy and native sequences enter directly"
  (define ran 0)
  (define s (in-amb/do (begin (set! ran 1) (amb 1 (error "early branch")))))
  (same ran 0)
  (same (for/first ([x s]) x) 1)
  (same ran 1)
  (same (for/list ([_ (in-range 5)]
                   [x (in-amb/do (make-amb (λ () (sequence-generate* (in-naturals)))))]) x)
        '(0 1 2 3 4)))

(test-case "zero does not request the other side or a tail prematurely"
  (define (bomb) (error "early next"))
  (for ([merge mergers])
    (same (events (merge (λ () (values 0 (λ () (values '(first) bomb)))) bomb) 1)
          '((first)))))

(printf "~a comparisons; all observations bounded\n" comparisons)
