#lang racket/base

;; RESEARCH (ambKanren issue #14): `amb-output` (name provisional).
;; Run with: raco test pkgs/amb/tests/amb-output.rkt
(require rackunit racket/sequence racket/stream amb)

;; the pause idiom built from amb-output and amb
(define (suspend v) (when (amb #t #f) (amb-output v)))

(test-case "amb-output outputs once and abandons the rest"
  (check-equal? (for/list ([x (in-amb/do (begin (amb-output 'early) 'late))]) x)
                '(early))
  (check-equal? (stream->list (in-amb (let ([x (amb 1 2)]) (amb-output x) 'no)))
                '(1 2)))

(test-case "it is not failure: siblings still run, and #t / #f are just values"
  (check-equal? (for/list ([x (in-amb/do (amb (amb-output #t) (amb-output #f) 3))]) x)
                '(#t #f 3)))

(test-case "zero and several values use the ordinary multi-value leaves"
  (check-equal? (for/list ([(a b) (in-amb/do (amb (amb-output 1 2) (values 3 4)))])
                  (list a b))
                '((1 2) (3 4)))
  (check-equal? (for/list ([() (in-amb/do (amb (amb-output) (values)))]) 'tick)
                '(tick tick)))

(test-case "the pause idiom: output, then continue after it"
  (define log '())
  (define out
    (for/list ([x (in-amb/do (begin (set! log (cons 'before log))
                                    (suspend 'P)
                                    (set! log (cons 'after log))
                                    'x))])
      (set! log (cons (list 'got x) log))
      x))
  (check-equal? out '(P x))
  (check-equal? (reverse log) '(before (got P) after (got x))))

(test-case "an output reaches only the innermost search"
  (check-equal?
   (for/list ([x (in-amb/do
                  (let-values ([(more? get)
                                (sequence-generate
                                 (in-amb/do (begin (suspend 'inner-P) 'inner)))])
                    (list (get) (get))))])
     x)
   '((inner-P inner))))

(test-case "answer sets are unchanged under every strategy"
  (for* ([df '(#t #f)] [fair '(#t #f)])
    (define vs
      (parameterize ([current-amb-depth-first? df] [current-amb-fair? fair])
        (for/list ([v (in-amb/do (let ([x (amb 1 2 3)]) (suspend 'P) x))]
                   #:unless (eq? v 'P))
          v)))
    (check-equal? (sort vs <) '(1 2 3) (format "df=~a fair=~a" df fair))))

(test-case "a computation that keeps pausing yields control each time"
  (define (spin) (suspend 'P) (spin))
  (define-values (more? get) (sequence-generate (in-amb/do (spin))))
  (check-equal? (for/list ([_ 5]) (and (more?) (get))) '(P P P P P)))

(test-case "outside any search it is an error, like amb"
  (check-exn exn:fail? (λ () (amb-output 1))))
