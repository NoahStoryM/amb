#lang racket/base

;; RESEARCH (ambKanren issue #14): `amb-fail`, `amb-empty`, `#:empty-values`.
;; Run with: raco test pkgs/amb/tests/amb-empty.rkt
(require rackunit racket/sequence racket/stream amb)

;; the pause idiom built from amb-empty and amb
(define (suspend) (when (amb #t #f) (amb-empty)))

(test-case "(amb-fail) fails like (amb)"
  (check-equal? (for/list ([x (in-amb/do (amb (amb-fail) 1 (amb) 2))]) x) '(1 2))
  (check-equal? (stream->list (in-amb (amb (amb) 5))) '(5)))

(test-case "without #:empty-values, (amb-empty) is (amb-fail)"
  (check-equal? (for/list ([x (in-amb/do (amb (amb-empty) 1))]) x) '(1))
  ;; so the pause idiom only reschedules: nothing is lost or added
  (check-equal? (for/list ([x (in-amb/do (begin (suspend) (amb 1 (begin (suspend) 2))))]) x)
                '(1 2)))

(test-case "with #:empty-values, (amb-empty) outputs them and abandons the rest"
  (check-equal? (for/list ([x (in-amb/do (begin (amb-empty) 'late) #:empty-values '(P))]) x)
                '(P))
  (check-equal? (stream->list (in-amb (let ([x (amb 1 2)]) (amb-empty) x) #:empty-values '(P)))
                '(P P)))

(test-case "(amb-fail) is never an output, even with #:empty-values"
  (check-equal? (for/list ([x (in-amb/do (amb (amb-fail) 1) #:empty-values '(P))]) x) '(1)))

(test-case "function forms take the empty values as an optional argument"
  (check-equal? (for/list ([x (in-amb*/do (λ () (suspend) 'x) '(P))]) x) '(P x))
  (check-equal? (stream->list (in-amb* (λ () (suspend) 'x) '(P))) '(P x))
  (check-equal? (for/list ([x (in-amb* (λ () (suspend) 'x) '(P))]) x) '(P x))
  (check-equal? (for/list ([x (in-amb (begin (suspend) 'x) #:empty-values '(P))]) x) '(P x))
  (check-equal? (for/list ([x (in-amb*/do (λ () (suspend) 'x) #f)]) x) '(x)))

(test-case "zero and several values use the ordinary multi-value leaves"
  (check-equal? (for/list ([(a b) (in-amb/do (begin (suspend) (values 3 4)) #:empty-values '(1 2))])
                  (list a b))
                '((1 2) (3 4)))
  (check-equal? (for/list ([() (in-amb/do (begin (suspend) (values)) #:empty-values '())]) 'tick)
                '(tick tick)))

(test-case "the pause idiom: output, then continue after it"
  (define log '())
  (define out
    (for/list ([x (in-amb/do (begin (set! log (cons 'before log))
                                    (suspend)
                                    (set! log (cons 'after log))
                                    'x)
                             #:empty-values '(P))])
      (set! log (cons (list 'got x) log))
      x))
  (check-equal? out '(P x))
  (check-equal? (reverse log) '(before (got P) after (got x))))

(test-case "each search uses its own empty values; an inner search without them hides pauses"
  (check-equal?
   (for/list ([x (in-amb/do
                  (let-values ([(more? get)
                                (sequence-generate
                                 (in-amb/do (begin (suspend) 'inner) #:empty-values '(inner-P)))])
                    (suspend)
                    (list (get) (get)))
                  #:empty-values '(outer-P))])
     x)
   '(outer-P (inner-P inner)))
  ;; a plain inner search (no empty values) just reschedules: the answer survives
  (check-equal?
   (for/list ([x (in-amb/do
                  (let-values ([(more? get)
                                (sequence-generate (in-amb/do (begin (suspend) 'inner)))])
                    (list (and (more?) (get))))
                  #:empty-values '(outer-P))])
     x)
   '((inner))))

(test-case "the empty values are not dynamically scoped"
  ;; resuming from a different context does not change what is output
  (define-values (more? get)
    (sequence-generate (in-amb/do (begin (suspend) (suspend) 'x) #:empty-values '(P))))
  (check-equal? (list (get) (get) (get)) '(P P x)))

(test-case "answer sets are unchanged under every strategy"
  (for* ([df '(#t #f)] [fair '(#t #f)])
    (define vs
      (parameterize ([current-amb-depth-first? df] [current-amb-fair? fair])
        (for/list ([v (in-amb/do (let ([x (amb 1 2 3)]) (suspend) x) #:empty-values '(P))]
                   #:unless (eq? v 'P))
          v)))
    (check-equal? (sort vs <) '(1 2 3) (format "df=~a fair=~a" df fair))))

(test-case "a computation that keeps pausing yields control each time"
  (define (spin) (suspend) (spin))
  (define-values (more? get) (sequence-generate (in-amb/do (spin) #:empty-values '(P))))
  (check-equal? (for/list ([_ 5]) (and (more?) (get))) '(P P P P P)))

(test-case "the empty values must be #f or a list"
  (check-exn exn:fail:contract? (λ () (in-amb* (λ () 1) 'P)))
  (check-exn exn:fail:contract? (λ () (for/list ([x (in-amb* (λ () 1) 'P)]) x))))

(test-case "(amb) and (for/amb #:length 0 ...) go through the shuffler"
  (define calls 0)
  (parameterize ([current-amb-shuffler (λ (v) (set! calls (add1 calls)))])
    (check-equal? (for/list ([x (in-amb/do (amb))]) x) '())
    (check-equal? (for/list ([x (in-amb/do (for/amb #:length 0 ([i '()]) i))]) x) '()))
  (check-equal? calls 2))

(test-case "for/amb #:length evaluates its length once"
  (define n 0)
  (check-equal? (for/list ([x (in-amb/do (for/amb #:length (begin (set! n (add1 n)) 2)
                                           ([i '(a b)]) i))])
                  x)
                '(a b))
  (check-equal? n 1))

(test-case "outside any search they are errors, like amb"
  (check-exn exn:fail? (λ () (amb-empty)))
  (check-exn exn:fail? (λ () (amb-fail))))
