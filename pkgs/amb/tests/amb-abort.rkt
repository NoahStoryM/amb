#lang racket/base

;; RESEARCH (ambKanren issue #14): `amb-abort`.
;; Run with: raco test pkgs/amb/tests/amb-abort.rkt
(require rackunit racket/sequence racket/stream amb)

;; the pause idiom built from amb-abort and amb
(define (suspend v) (when (amb #t #f) (amb-abort (list v))))

(test-case "(amb-abort) and (amb-abort #f) fail like (amb)"
  (check-equal? (for/list ([x (in-amb/do (amb (amb-abort) 1 (amb-abort #f) 2 (amb)))]) x)
                '(1 2))
  (check-equal? (stream->list (in-amb (amb (amb) 5))) '(5)))

(test-case "(amb-abort vs) outputs once and abandons the rest"
  (check-equal? (for/list ([x (in-amb/do (begin (amb-abort '(early)) 'late))]) x)
                '(early))
  (check-equal? (stream->list (in-amb (let ([x (amb 1 2)]) (amb-abort (list x)) 'no)))
                '(1 2)))

(test-case "an output is not a failure: siblings still run"
  (check-equal? (for/list ([x (in-amb/do (amb (amb-abort '(#t)) (amb-abort '(#f)) 3))]) x)
                '(#t #f 3)))

(test-case "zero and several values use the ordinary multi-value leaves"
  (check-equal? (for/list ([(a b) (in-amb/do (amb (amb-abort '(1 2)) (values 3 4)))])
                  (list a b))
                '((1 2) (3 4)))
  ;; '() is a zero-valued OUTPUT, not a failure
  (check-equal? (for/list ([() (in-amb/do (amb (amb-abort '()) (values)))]) 'tick)
                '(tick tick)))

(test-case "the contract accepts only #f or a list"
  (check-exn exn:fail:contract? (λ () (for/list ([x (in-amb/do (amb-abort #t))]) x)))
  (check-exn exn:fail:contract? (λ () (for/list ([x (in-amb/do (amb-abort 'P))]) x))))

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

(test-case "outside any search it is an error, like amb"
  (check-exn exn:fail? (λ () (amb-abort '(1)))))
