#lang racket/base

;; RESEARCH (ambKanren issue #14): `amb-fail` and the `#:empty` handler
;; for `(amb)`.  Run with: raco test pkgs/amb/tests/amb-handler.rkt
(require rackunit racket/sequence racket/stream amb)

;; the pause idiom: (amb) is answered by the search's handler
(define (suspend) (when (amb #t #f) (amb)))
(define (P) 'P)

(test-case "by default (amb) is (amb-fail)"
  (check-equal? (for/list ([x (in-amb/do (amb (amb-fail) 1 (amb) 2))]) x) '(1 2))
  (check-equal? (stream->list (in-amb (amb (amb) 5))) '(5))
  ;; so the pause idiom only reschedules: nothing is lost or added
  (check-equal? (for/list ([x (in-amb/do (begin (suspend) (amb 1 (begin (suspend) 2))))]) x)
                '(1 2)))

(test-case "with #:empty, (amb) outputs the handler's values and abandons the rest"
  (check-equal? (for/list ([x (in-amb/do (begin (amb) 'late) #:empty P)]) x) '(P))
  (check-equal? (stream->list (in-amb (let ([x (amb 1 2)]) (amb) x) #:empty P)) '(P P)))

(test-case "(amb-fail) is never answered by the handler"
  (check-equal? (for/list ([x (in-amb/do (amb (amb-fail) 1) #:empty P)]) x) '(1)))

(test-case "a choice with no alternatives is (amb)"
  (check-equal? (for/list ([x (in-amb/do (for/amb ([i '()]) i) #:empty P)]) x) '(P))
  (check-equal? (for/list ([x (in-amb/do (for/amb #:length 0 ([i '()]) i) #:empty P)]) x) '(P))
  (check-equal? (for/list ([x (in-amb/do (make-amb (λ () #f) void) #:empty P)]) x) '(P))
  ;; but running out after some alternatives is not
  (check-equal? (for/list ([x (in-amb/do (for/amb ([i '(1 2)]) i) #:empty P)]) x) '(1 2))
  (check-equal? (for/list ([x (in-amb/do (amb 1 2) #:empty P)]) x) '(1 2)))

(test-case "the handler runs where (amb) is called"
  (define p (make-parameter 'outside))
  (check-equal? (for/list ([x (in-amb/do (parameterize ([p 'inside]) (amb)) #:empty p)]) x)
                '(inside))
  ;; and may fail or raise
  (check-equal? (for/list ([x (in-amb/do (amb (amb) 1) #:empty amb-fail)]) x) '(1))
  (check-exn #rx"boom" (λ () (for/list ([x (in-amb/do (amb) #:empty (λ () (error "boom")))]) x))))

(test-case "function forms take the handler as an optional argument"
  (check-equal? (for/list ([x (in-amb*/do (λ () (suspend) 'x) P)]) x) '(P x))
  (check-equal? (stream->list (in-amb* (λ () (suspend) 'x) P)) '(P x))
  (check-equal? (for/list ([x (in-amb* (λ () (suspend) 'x) P)]) x) '(P x))
  (check-equal? (for/list ([x (in-amb (begin (suspend) 'x) #:empty P)]) x) '(P x))
  (check-equal? (for/list ([x (in-amb*/do (λ () (suspend) 'x) amb-fail)]) x) '(x)))

(test-case "zero and several values"
  (check-equal? (for/list ([(a b) (in-amb/do (begin (suspend) (values 3 4)) #:empty (λ () (values 1 2)))])
                  (list a b))
                '((1 2) (3 4)))
  (check-equal? (for/list ([() (in-amb/do (begin (suspend) (values)) #:empty values)]) 'tick)
                '(tick tick)))

(test-case "the pause idiom: output, then continue after it"
  (define log '())
  (define out
    (for/list ([x (in-amb/do (begin (set! log (cons 'before log))
                                    (suspend)
                                    (set! log (cons 'after log))
                                    'x)
                             #:empty P)])
      (set! log (cons (list 'got x) log))
      x))
  (check-equal? out '(P x))
  (check-equal? (reverse log) '(before (got P) after (got x))))

(test-case "each search uses its own handler; an inner search without one hides pauses"
  (check-equal?
   (for/list ([x (in-amb/do
                  (let-values ([(more? get)
                                (sequence-generate
                                 (in-amb/do (begin (suspend) 'inner) #:empty (λ () 'inner-P)))])
                    (suspend)
                    (list (get) (get)))
                  #:empty (λ () 'outer-P))])
     x)
   '(outer-P (inner-P inner)))
  ;; a plain inner search (no handler) just reschedules: the answer survives
  (check-equal?
   (for/list ([x (in-amb/do
                  (let-values ([(more? get)
                                (sequence-generate (in-amb/do (begin (suspend) 'inner)))])
                    (list (and (more?) (get))))
                  #:empty (λ () 'outer-P))])
     x)
   '((inner))))

(test-case "the handler is the search's, wherever its trees are resumed"
  (define-values (more? get)
    (sequence-generate (in-amb/do (begin (suspend) (suspend) 'x) #:empty P)))
  ;; resumed from inside another search that has a different handler
  (check-equal? (for/list ([_ 3])
                  (for/first ([v (in-amb/do (get) #:empty (λ () 'other))]) v))
                '(P P x)))

(test-case "answer sets are unchanged under every strategy"
  (for* ([df '(#t #f)] [fair '(#t #f)])
    (define vs
      (parameterize ([current-amb-depth-first? df] [current-amb-fair? fair])
        (for/list ([v (in-amb/do (let ([x (amb 1 2 3)]) (suspend) x) #:empty P)]
                   #:unless (eq? v 'P))
          v)))
    (check-equal? (sort vs <) '(1 2 3) (format "df=~a fair=~a" df fair))))

(test-case "a computation that keeps pausing yields control each time"
  (define (spin) (suspend) (spin))
  (define-values (more? get) (sequence-generate (in-amb/do (spin) #:empty P)))
  (check-equal? (for/list ([_ 5]) (and (more?) (get))) '(P P P P P)))

(test-case "the handler must be a thunk"
  (check-exn exn:fail:contract? (λ () (in-amb* (λ () 1) 'P)))
  (check-exn exn:fail:contract? (λ () (for/list ([x (in-amb* (λ () 1) 'P)]) x))))

(test-case "(amb) and (for/amb #:length 0 ...) go through the shuffler"
  (define calls 0)
  (parameterize ([current-amb-shuffler (λ (v) (set! calls (add1 calls)))])
    (check-equal? (for/list ([x (in-amb/do (amb))]) x) '())
    (check-equal? (for/list ([x (in-amb/do (for/amb #:length 0 ([i '()]) i))]) x) '()))
  (check-equal? calls 2))

(test-case "outside any search they are errors"
  (check-exn exn:fail? (λ () (amb)))
  (check-exn exn:fail? (λ () (amb-fail))))
