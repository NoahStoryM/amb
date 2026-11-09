#lang racket/base

;; RESEARCH (ambKanren issue #14): `current-amb-empty-values`.
;; Run with: raco test pkgs/amb/tests/amb-empty-values.rkt
(require rackunit racket/sequence racket/stream amb)

(define-syntax-rule (with-ev vs body ...)
  (parameterize ([current-amb-empty-values vs]) body ...))

(test-case "default: (amb) still fails"
  (check-equal? (for/list ([x (in-amb/do (begin (amb) 42))]) x) '())
  (check-equal? (stream->list (in-amb (amb (amb) 5))) '(5)))

(test-case "the example from the issue"
  (check-equal? (with-ev '(99) (for/list ([x (in-amb/do (begin (amb) 42))]) x))
                '(99 42)))

(test-case "the leaf is a separate output; execution resumes after (amb)"
  (define log '())
  (define out
    (for/list ([x (in-amb/do
                   (begin (set! log (cons 'before log))
                          (with-ev '(P) (amb))
                          (set! log (cons 'after log))
                          'x))])
      (set! log (cons (list 'got x) log))
      x))
  (check-equal? out '(P x))
  (check-equal? (reverse log) '(before (got P) after (got x))))

(test-case "a local parameterize does not leak past (amb): later (amb) fails"
  (check-equal?
   (for/list ([x (in-amb/do (begin (with-ev '(P) (amb)) (amb) 'unreachable))]) x)
   '(P)))

(test-case "after resumption, code inside the parameterize still sees it"
  (check-equal?
   (for/list ([x (in-amb/do (with-ev '(P) (amb) (current-amb-empty-values)))]) x)
   '(P (P))))

(test-case "after the parameterize, the CONSUMER's binding at resume time is seen"
  ;; The resumed continuation is composed onto the context of whoever
  ;; asks for the next element.
  (define-values (more? get)
    (sequence-generate
     (in-amb/do (begin (with-ev '(P) (amb)) (current-amb-empty-values)))))
  (define first (with-ev '(A) (and (more?) (get))))
  (define second (with-ev '(B) (and (more?) (get))))
  (check-equal? (list first second) '(P (B))))

(test-case "zero and several values use the ordinary multi-value leaves"
  (check-equal?
   (for/list ([(a b) (in-amb/do (begin (with-ev '(1 2) (amb)) (values 3 4)))])
     (list a b))
   '((1 2) (3 4)))
  (check-equal?
   (for/list ([() (in-amb/do (begin (with-ev '() (amb)) (values)))]) 'tick)
   '(tick tick)))

;; When resumed, (amb) RETURNS (void): in tail position that is a value.
(test-case "after resumption (amb) returns (void)"
  (check-equal? (for/list ([x (in-amb/do (with-ev '(P) (amb)))]) x)
                (list 'P (void))))

(test-case "other failure primitives ignore the parameter"
  (check-equal? (with-ev '(P) (for/list ([x (in-amb/do (make-amb (λ () #f) (λ () 1)))]) x))
                '())
  (check-equal? (with-ev '(P) (for/list ([x (in-amb/do (for/amb ([i '()]) i))]) x))
                '()))

(test-case "setting it around the whole search turns EVERY (amb) into an output"
  ;; ... and the empty alternative then continues with (void) as its value
  (check-equal? (with-ev '(P) (for/list ([x (in-amb/do (amb 1 (amb) 2))]) x))
                (list 1 'P (void) 2)))

(test-case "the leaf reaches only the innermost search"
  (check-equal?
   (for/list ([x (in-amb/do
                  (let-values ([(more? get)
                                (sequence-generate
                                 (in-amb/do (begin (with-ev '(inner-P) (amb)) 'inner)))])
                    (list (get) (get))))])
     x)
   '((inner-P inner))))

(test-case "answer sets are unchanged under every strategy"
  (for* ([df '(#t #f)] [fair '(#t #f)])
    (define vs
      (parameterize ([current-amb-depth-first? df] [current-amb-fair? fair])
        (for/list ([v (in-amb/do (let ([x (amb 1 2 3)]) (with-ev '(P) (amb)) x))]
                   #:unless (eq? v 'P))
          v)))
    (check-equal? (sort vs <) '(1 2 3) (format "df=~a fair=~a" df fair))))

(test-case "a computation that keeps doing it yields control each time"
  (define (spin) (with-ev '(P) (amb)) (spin))
  (define-values (more? get) (sequence-generate (in-amb/do (spin))))
  (check-equal? (for/list ([_ 5]) (and (more?) (get))) '(P P P P P)))
