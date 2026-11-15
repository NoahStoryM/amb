#lang typed/racket/base

;; The typed interface of v3: generators, mergers, `amb-generate*`.
(require typed/amb typed/amb/merge typed/rackunit
         (prefix-in base: typed/amb/base))

(define-type Events (Listof (∪ Integer Symbol)))
(define-syntax-rule (events e)
  (for/list : Events ([x (in-amb/do e #:on-pause (λ () 'P))]) x))

;; a generator from its results
(: source (→ (∪ (List Integer) #f Positive-Integer) * (Generatorof (List Integer))))
(define ((source . rs))
  (if (null? rs)
      (values #f (source))
      (values (car rs) (apply source (cdr rs)))))

(test-case "generators"
  (check-equal? (events (make-amb (source '(1) 2 '(2)))) '(1 P P 2))
  ;; several values
  (check-equal? (for/list : (Listof (List Integer Symbol))
                  ([(a b) (in-amb/do ((inst make-amb Integer Symbol)
                                      (λ () (values (list 1 'x)
                                                    (λ () (values #f (λ () (error "the end"))))))))])
                  (list a b))
                '((1 x)))
  (check-equal? (events (sequence->amb (list 1 2 3))) '(1 2 3)))

(test-case "mergers"
  ;; a merger of one's own, which handles n pauses
  (: tick-append Merger)
  (define ((tick-append s t)) (values 1 (amb-append s t)))
  (check-equal? (parameterize ([current-amb-merger tick-append]) (events (amb 1 2)))
                '(P 1 P 2))
  (for ([m : Merger (list amb-append amb-interleave amb-alternate amb-level)])
    ;; (the type of a merger, and what it rules out: merger-types.rkt)
    (check-equal? (sort (parameterize ([current-amb-merger m])
                          (for/list : (Listof Integer)
                            ([x (in-amb (let ([x (amb 1 2 3)]) (amb-pause) x))])
                            x))
                        <)
                  '(1 2 3)))
  ;; the standard mergers keep the type of the generators
  (define g : (Generatorof (List Integer)) (amb-interleave (source 1 '(1)) (source '(2))))
  (check-equal? (events (make-amb g)) '(P 2 1))
  (check-equal? (events (base:make-amb (source '(7)))) '(7)))

(test-case "amb-generate*"
  (define-values (v next) (amb-generate* (λ () : Integer (amb 1 (begin (amb-pause) 2)))))
  (check-equal? v '(1))
  (define-values (v2 next2) (next))
  (check-equal? v2 1)
  (define-values (v3 next3) (next2))
  (check-equal? v3 '(2))
  ;; a local search
  (check-equal? (events (make-amb (λ () (amb-generate* (λ () : Integer (amb 1 (begin (amb-pause) 2)))))))
                '(1 P 2))
  (check-equal? (events (amb-local (amb 1 (begin (amb-pause) 2)))) '(1 P 2))
  (check-equal? (for/list : (Listof (List Integer Symbol))
                  ([(a b) (in-amb (amb-local (amb (values 1 'x) (values 2 'y))))])
                  (list a b))
                '((1 x) (2 y))))

(test-case "amb-local* preserves the thunk's result types"
  (check-equal? (events (amb-local* (λ () : Integer (amb 1 (begin (amb-pause) 2)))))
                '(1 P 2))
  (check-equal? (for/list : (Listof (List Integer Symbol))
                  ([(a b) (in-amb (amb-local* (λ () (values 1 'x))))])
                  (list a b))
                '((1 x)))
  (: zero-local (→ Integer))
  (define (zero-local)
    (define-values () ((inst amb-local*) (λ () (values))))
    7)
  (check-equal? (events (zero-local)) '(7))
  (check-equal? (events ((inst amb-local* Integer) (λ () : Integer (amb-fail)))) '()))

(test-case "with-amb-mark forwards zero or multiple values"
  (define-values (a b) (with-amb-mark (values 1 'x)))
  (check-equal? (list a b) '(1 x))
  (define-values (c d) (call-with-amb-mark (λ () (values 2 'y))))
  (check-equal? (list c d) '(2 y))
  (define-values () (with-amb-mark (values)))
  (check-equal? (events (begin (with-amb-mark (void)) (amb 1 2))) '(1 2)))

(test-case "the shuffler"
  (define sizes : (Listof Integer) '())
  (parameterize ([current-amb-shuffler
                  (λ ([v : Mutable-VectorTop]) (set! sizes (cons (vector-length v) sizes)))])
    (check-equal? (events (amb 1 2 3)) '(1 2 3))
    (check-equal? (events (begin (amb) 1)) '())
    (check-equal? (events (for/amb : Integer #:length 5 ([i : Integer '(1 2)]) i)) '(1 2))
    (check-equal? (events (for/amb : Integer #:length 3 #:fill 0 ([i : Integer '(1 2)]) i)) '(1 2 0))
    (check-equal? (events (for/amb : Integer ([i : Integer '(1 2)]) i)) '(1 2)))
  (check-equal? (reverse sizes) '(3 0 2 3)))

(test-case "the supply of alternatives"
  ;; without #:length, as in the untyped library
  (check-exn #rx"make-amb: not allowed while the alternatives of a choice are supplied"
             (λ () (events (for/amb : Integer ([i : Integer '(1 2 3)] #:when (amb #t #f)) i))))
  ;; with #:length, and in the shuffler
  (check-exn #rx"make-amb: not allowed while the alternatives of a choice are supplied"
             (λ () (events (for/amb : Integer #:length 3 ([i : Integer '(1 2 3)] #:when (amb #t #f)) i))))
  (check-exn #rx"amb-pause: not allowed while the alternatives of a choice are supplied"
             (λ () (parameterize ([current-amb-shuffler (λ ([v : Mutable-VectorTop]) (amb-pause))])
                     (events (amb 1 2)))))
  ;; one's own
  (check-exn #rx"amb-fail: not allowed"
             (λ () (events (with-amb-mark (amb-fail)))))
  (check-equal? (events (call-with-amb-mark (λ () 1))) '(1)))

(test-case "length expressions use the same supply boundary as untyped for/amb"
  (check-exn #rx"make-amb: not allowed"
             (λ () (events (for/amb : Integer #:length (amb 1 2) ([x '(10 20)]) x))))
  (check-exn #rx"make-amb: not allowed"
             (λ () (events (for*/amb : Integer #:length (amb 1 2) ([x '(10 20)]) x))))
  (check-exn #rx"amb-pause: not allowed"
             (λ () (events (for/amb : Integer #:length (begin (amb-pause) 0) ([x '(10 20)]) x))))
  (check-exn #rx"amb-fail: not allowed"
             (λ () (events (for/amb : Integer #:length (begin (amb-fail) 0) ([x '(10 20)]) x))))
  (check-exn #rx"make-amb: not allowed"
             (λ () (events (for/amb : Integer #:length (amb 0 1) #:fill 99 ([x '(10 20)]) x))))
  (check-exn #rx"amb-pause: not allowed"
             (λ () (events (for*/amb : Integer #:length (begin (amb-pause) 0)
                             #:fill 99 ([x '(10 20)]) x))))
  (define lengths : Integer 0)
  (check-equal? (events (for/amb : Integer #:length (begin (set! lengths (add1 lengths)) 0)
                          #:fill (error "early fill")
                          ([x : Integer (error "early clause")]) x))
                '())
  (check-equal? lengths 1)
  (check-equal? (events (for/amb : Integer #:length (length (events (amb 1 2)))
                          ([x '(10 20)]) (amb x (- x))))
                '(10 -10 20 -20)))
