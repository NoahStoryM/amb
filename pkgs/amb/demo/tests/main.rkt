#lang racket/base

(require rackunit
         racket/list
         racket/sequence
         racket/stream
         amb/demo
         (prefix-in core: amb/demo/main)
         (prefix-in spec: "../../tests/spec-merge.rkt"))

(define mergers (list amb-append amb-interleave amb-alternate amb-level))
(define equations
  (list spec:amb-append spec:amb-interleave spec:amb-alternate spec:amb-level))

(define (source es)
  (λ ()
    (if (null? es)
        (values #f (λ () (error "terminal next called")))
        (values (car es) (source (cdr es))))))

;; Count logical events, including each pause of a batch. Never consume
;; an infinite generator without fuel.
(define (events g fuel)
  (cond
    [(zero? fuel) '()]
    [else
     (define-values (v next) (g))
     (cond
       [(not v) '(end)]
       [(list? v) (cons v (events next (sub1 fuel)))]
       [else
        (define n (min fuel v))
        (append (make-list n 'P)
                (if (= n fuel) '() (events next (- fuel n))))])]))

(test-case "the launcher exports both observer functions"
  (check-eq? in-amb* core:in-amb*)
  (check-eq? in-amb*/do core:in-amb*/do)
  (check-eq? current-amb-merger core:current-amb-merger)
  (check-equal? (stream->list (in-amb* (λ () (amb 1 2)))) '(1 2))
  (check-equal? (for/list ([v (in-amb*/do (λ () (amb 1 2)))]) v) '(1 2)))

(test-case "four mergers agree with the equations at equal fuel"
  (define words '(() ((a)) (1 (b)) (2 (c) 1) ((d) 3 (e)) (() (1 2))))
  (for ([merge mergers] [equation equations])
    (for* ([left words] [right words] [fuel '(0 1 3 20)])
      (check-equal? (events (merge (source left) (source right)) fuel)
                    (events (equation (source left) (source right)) fuel)))))

(test-case "choices share their continuation; failure has no pause"
  (check-equal? (stream->list (in-amb (list (amb 1 2) (amb 'a 'b))))
                '((1 a) (1 b) (2 a) (2 b)))
  (check-equal? (stream->list (in-amb (amb (amb-fail) 1 (amb) 2)
                                     #:on-pause (λ () 'P)))
                '(1 2))
  (define ((tick-append s t)) (values 1 (amb-append s t)))
  (parameterize ([current-amb-merger tick-append])
    (check-equal? (stream->list (in-amb (amb 'a 'b) #:on-pause (λ () 'P)))
                  '(P a P b))))

(test-case "native sequence generators supply alternatives directly"
  (check-equal? (stream->list (in-amb (make-amb (λ () (sequence-generate* (in-range 4))))))
                '(0 1 2 3))
  (check-equal? (stream->list (in-amb (sequence->amb (in-list '(a b c))))) '(a b c))
  (check-equal? (for/list ([_ (in-range 5)]
                         [v (in-amb/do (make-amb (λ () (sequence-generate* (in-naturals)))))])
                  v)
                '(0 1 2 3 4))
  (check-equal? (for/list ([(a b) (in-amb/do
                                  (make-amb
                                   (λ () (sequence-generate*
                                          (in-parallel (in-range 3) (in-list '(a b c)))))))])
                  (list a b))
                '((0 a) (1 b) (2 c))))

(test-case "answers and pause observations keep zero and multiple values"
  (for ([observe (list in-amb* in-amb*/do)])
    (check-equal? (sequence->list
                  (sequence-map list (observe (λ () (amb 0 (values) (values 1 2))))))
                  '((0) () (1 2)))
    (check-equal? (sequence->list
                  (sequence-map list (observe (λ () (amb-pause) (values 3 4)) values)))
                  '(() (3 4)))))

(test-case "cached observations are lazy and run handlers once"
  (define starts 0)
  (define answers 0)
  (define pauses 0)
  (define s
    (in-amb*
     (λ ()
       (set! starts (add1 starts))
       (amb-pause)
       (amb (begin (set! answers (add1 answers)) 1)
            (begin (set! answers (add1 answers)) 2)))
     (λ () (set! pauses (add1 pauses)) 'P)))
  (check-true (stream? s))
  (check-equal? (list starts answers pauses) '(0 0 0))
  (check-equal? (stream-first s) 'P)
  (check-equal? (stream-first s) 'P)
  (check-equal? (list starts answers pauses) '(1 0 1))
  (check-equal? (stream->list s) '(P 1 2))
  (check-equal? (stream->list s) '(P 1 2))
  (check-equal? (list starts answers pauses) '(1 2 1))
  (check-equal? (stream->list (in-amb (begin (amb-pause) 42) #:on-pause (λ () 'P)))
                '(P 42)))

(test-case "single-pass observations share a cursor and stay exhausted"
  (define s (in-amb*/do (λ () (amb 1 2 3))))
  (check-equal? (for/first ([v s]) v) 1)
  (check-equal? (for/list ([v s]) v) '(2 3))
  (check-equal? (for/list ([v s]) v) '())
  (check-equal? (for/list ([v s]) v) '())
  (check-equal? (for/list ([v (in-amb/do (amb-fail))]) v) '())
  (check-equal? (for/first ([v (in-amb/do (amb 1 (error "early branch")))]) v) 1))

(test-case "iteration bodies are delayed; parallel and nested bindings work"
  (check-equal? (stream->list (in-amb (for/amb ([i '(1 2)] [j '(a b)]) (list i j))))
                '((1 a) (2 b)))
  (check-equal? (stream->list (in-amb (for*/amb ([i '(1 2)] [j (in-range i)])
                                     #:final #f (list i j))))
                '((1 0) (2 0) (2 1)))
  (check-equal? (stream->list (in-amb (for*/amb ([i '(1 2)] [j (in-range i)])
                                     #:break (and (= i 2) (= j 1)) (list i j))))
                '((1 0) (2 0)))
  (check-equal? (stream->list (in-amb (for/amb ([i '(1 2)]) (amb i (- i)))))
                '(1 -1 2 -2)))

(test-case "batch pauses and infinite pauses are observed with finite fuel"
  (check-equal? (stream->list (in-amb (make-amb (source '(3 (42) 2)))
                                     #:on-pause (λ () 'P)))
                '(P P P 42 P P))
  (define (never) (amb-pause) (never))
  (parameterize ([current-amb-merger amb-interleave])
    (check-equal? (for/list ([_ (in-range 8)]
                           [v (in-amb/do (amb (never) 'x) #:on-pause (λ () 'P))])
                    v)
                  '(P x P P P P P P))))

(test-case "a raw zero is a protocol error, not an element or a pause"
  (for ([merge mergers])
    (check-exn #rx"given: 0" (λ () ((merge (source '(0)) (source '()))))))
  (check-exn #rx"given: 0"
             (λ () (stream-first (in-amb (make-amb (source '(0)))))))
  (define ((zero-first s t)) (values 0 s))
  (parameterize ([current-amb-merger zero-first])
    (check-exn #rx"given: 0" (λ () (stream-first (in-amb (amb 42)))))))
