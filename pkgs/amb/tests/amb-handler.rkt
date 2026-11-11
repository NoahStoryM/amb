#lang racket/base

;; `amb-fail` and the `#:empty-handler` handler for `(amb)`.  Run with: raco test pkgs/amb/tests/amb-handler.rkt
(require rackunit racket/sequence racket/stream amb)

;; the pause idiom: (amb) is answered by the search's handler
(define (suspend) (when (amb #t #f) (amb)))
(define (P) 'P)
(define (vector-reverse! v)
  (define n (vector-length v))
  (for ([i (quotient n 2)])
    (define t (vector-ref v i))
    (vector-set! v i (vector-ref v (- n i 1)))
    (vector-set! v (- n i 1) t)))

(test-case "by default (amb) is (amb-fail)"
  (check-equal? (for/list ([x (in-amb/do (amb (amb-fail) 1 (amb) 2))]) x) '(1 2))
  (check-equal? (stream->list (in-amb (amb (amb) 5))) '(5))
  ;; so the pause idiom only reschedules: nothing is lost or added
  (check-equal? (for/list ([x (in-amb/do (begin (suspend) (amb 1 (begin (suspend) 2))))]) x)
                '(1 2)))

(test-case "with #:empty-handler, (amb) outputs the handler's values and abandons the rest"
  (check-equal? (for/list ([x (in-amb/do (begin (amb) 'late) #:empty-handler P)]) x) '(P))
  (check-equal? (stream->list (in-amb (let ([x (amb 1 2)]) (amb) x) #:empty-handler P)) '(P P)))

(test-case "(amb-fail) is never answered by the handler"
  (check-equal? (for/list ([x (in-amb/do (amb (amb-fail) 1) #:empty-handler P)]) x) '(1)))

(test-case "a choice with no alternatives is (amb)"
  (check-equal? (for/list ([x (in-amb/do (for/amb ([i '()]) i) #:empty-handler P)]) x) '(P))
  (check-equal? (for/list ([x (in-amb/do (for/amb #:length 0 ([i '()]) i) #:empty-handler P)]) x) '(P))
  (check-equal? (for/list ([x (in-amb/do (make-amb (λ () #f) void) #:empty-handler P)]) x) '(P))
  ;; but running out after some alternatives is not
  (check-equal? (for/list ([x (in-amb/do (for/amb ([i '(1 2)]) i) #:empty-handler P)]) x) '(1 2))
  (check-equal? (for/list ([x (in-amb/do (amb 1 2) #:empty-handler P)]) x) '(1 2)))

(test-case "the handler runs where (amb) is called"
  (define p (make-parameter 'outside))
  (check-equal? (for/list ([x (in-amb/do (parameterize ([p 'inside]) (amb)) #:empty-handler p)]) x)
                '(inside))
  ;; and may fail or raise
  (check-equal? (for/list ([x (in-amb/do (amb (amb) 1) #:empty-handler amb-fail)]) x) '(1))
  (check-exn #rx"boom" (λ () (for/list ([x (in-amb/do (amb) #:empty-handler (λ () (error "boom")))]) x))))

(test-case "function forms take the handler as an optional argument"
  (check-equal? (for/list ([x (in-amb*/do (λ () (suspend) 'x) P)]) x) '(P x))
  (check-equal? (stream->list (in-amb* (λ () (suspend) 'x) P)) '(P x))
  (check-equal? (for/list ([x (in-amb* (λ () (suspend) 'x) P)]) x) '(P x))
  (check-equal? (for/list ([x (in-amb (begin (suspend) 'x) #:empty-handler P)]) x) '(P x))
  (check-equal? (for/list ([x (in-amb*/do (λ () (suspend) 'x) amb-fail)]) x) '(x)))

(test-case "zero and several values"
  (check-equal? (for/list ([(a b) (in-amb/do (begin (suspend) (values 3 4)) #:empty-handler (λ () (values 1 2)))])
                  (list a b))
                '((1 2) (3 4)))
  (check-equal? (for/list ([() (in-amb/do (begin (suspend) (values)) #:empty-handler values)]) 'tick)
                '(tick tick)))

(test-case "the pause idiom: output, then continue after it"
  (define log '())
  (define out
    (for/list ([x (in-amb/do (begin (set! log (cons 'before log))
                                    (suspend)
                                    (set! log (cons 'after log))
                                    'x)
                             #:empty-handler P)])
      (set! log (cons (list 'got x) log))
      x))
  (check-equal? out '(P x))
  (check-equal? (reverse log) '(before (got P) after (got x))))

(test-case "each search uses its own handler; an inner search without one hides pauses"
  (check-equal?
   (for/list ([x (in-amb/do
                  (let-values ([(more? get)
                                (sequence-generate
                                 (in-amb/do (begin (suspend) 'inner) #:empty-handler (λ () 'inner-P)))])
                    (suspend)
                    (list (get) (get)))
                  #:empty-handler (λ () 'outer-P))])
     x)
   '(outer-P (inner-P inner)))
  ;; a plain inner search (no handler) just reschedules: the answer survives
  (check-equal?
   (for/list ([x (in-amb/do
                  (let-values ([(more? get)
                                (sequence-generate (in-amb/do (begin (suspend) 'inner)))])
                    (list (and (more?) (get))))
                  #:empty-handler (λ () 'outer-P))])
     x)
   '((inner))))

(test-case "the handler is the search's, wherever its trees are resumed"
  (define-values (more? get)
    (sequence-generate (in-amb/do (begin (suspend) (suspend) 'x) #:empty-handler P)))
  ;; resumed from inside another search that has a different handler
  (check-equal? (for/list ([_ 3])
                  (for/first ([v (in-amb/do (get) #:empty-handler (λ () 'other))]) v))
                '(P P x)))

(test-case "answer sets are unchanged under every strategy"
  (for* ([df '(#t #f)] [fair '(#t #f)])
    (define vs
      (parameterize ([current-amb-depth-first? df] [current-amb-fair? fair])
        (for/list ([v (in-amb/do (let ([x (amb 1 2 3)]) (suspend) x) #:empty-handler P)]
                   #:unless (eq? v 'P))
          v)))
    (check-equal? (sort vs <) '(1 2 3) (format "df=~a fair=~a" df fair))))

(test-case "a computation that keeps pausing yields control each time"
  (define (spin) (suspend) (spin))
  (define-values (more? get) (sequence-generate (in-amb/do (spin) #:empty-handler P)))
  (check-equal? (for/list ([_ 5]) (and (more?) (get))) '(P P P P P)))

(test-case "the handler must be a thunk"
  (check-exn exn:fail:contract? (λ () (in-amb* (λ () 1) 'P)))
  (check-exn exn:fail:contract? (λ () (for/list ([x (in-amb* (λ () 1) 'P)]) x))))

(test-case "a choice with no alternative does not call the shuffler"
  (define calls 0)
  (parameterize ([current-amb-shuffler
                  (λ (v) (when (zero? (vector-length v)) (set! calls (add1 calls))))])
    (check-equal? (for/list ([x (in-amb/do (amb))]) x) '())
    (check-equal? (for/list ([x (in-amb/do (for/amb #:length 0 ([i '()]) i))]) x) '())
    (check-equal? (stream->list (in-amb (amb (amb) calls))) '(0)))
  (check-equal? calls 0))

(test-case "an empty for/amb evaluates its clauses once, with or without a handler"
  (define n 0)
  (check-equal? (for/list ([x (in-amb/do (for/amb ([i (begin (set! n (add1 n)) '())]) i))]) x) '())
  (check-equal? n 1)
  (set! n 0)
  (check-equal? (for/list ([x (in-amb/do (for*/amb ([i (begin (set! n (add1 n)) '())]) i) #:empty-handler P)]) x)
                '(P))
  (check-equal? n 1)
  ;; a second evaluation would not be empty: it must not happen
  (define src '())
  (check-equal? (for/list ([x (in-amb/do (for/amb ([i (begin0 src (set! src '(1 2)))]) i))]) x) '())
  (set! src '())
  (check-equal? (for/list ([x (in-amb/do (for/amb ([i (begin0 src (set! src '(1 2)))]) i) #:empty-handler P)]) x)
                '(P)))

(test-case "make-amb does not call more? again after it returned #f"
  (define m 0)
  (check-equal? (for/list ([x (in-amb/do (make-amb (λ () (set! m (add1 m)) #f) void))]) x) '())
  (check-equal? m 1)
  (define flip #f)
  (check-equal? (for/list ([x (in-amb/do (make-amb (λ () (begin0 flip (set! flip (not flip)))) (λ () 'got))
                                         #:empty-handler P)]
                           [_ 6])
                  x)
                '(P))
  (define k 0)
  (check-equal? (stream->list (in-amb (make-amb (λ () (set! k (add1 k)) (= k 2)) (λ () 'ghost)))) '()))

(test-case "an empty generator does not change the order of side effects (every strategy)"
  ;; expected values are those of amb 2.0, where an empty make-amb was a
  ;; dead end at once
  (for ([df   '(#t #t #f #f)]
        [fair '(#t #f #t #f)]
        [expected '((s 1 2) (s 1 2) (s 2 1) (s 1 2))]
        [expected-log '((probe open choose) (probe open choose)
                        (open probe choose) (open choose probe))])
    (define open? #f)
    (define rev? #f)
    (define log '())
    (define result
      (parameterize ([current-amb-depth-first? df] [current-amb-fair? fair])
        (stream->list
         (in-amb
          (amb (make-amb (λ () (set! log (cons 'probe log)) (when open? (set! rev? #t)) #f) void)
               (begin (set! log (cons 'open log)) (set! open? #t) 's)
               (begin (set! log (cons 'choose log)) (if rev? (amb 2 1) (amb 1 2))))))))
    (check-equal? result expected (format "df=~a fair=~a" df fair))
    (check-equal? (reverse log) expected-log (format "df=~a fair=~a" df fair))))

(test-case "#:length: only the default fill counts as no alternative"
  (check-equal? (for/list ([x (in-amb/do (for/amb #:length 2 #:fill 0 ([i '()]) i) #:empty-handler P)]) x) '(0 0))
  (check-equal? (for/list ([x (in-amb/do (for/amb #:length 2 #:fill (amb-fail) ([i '()]) i) #:empty-handler P)]) x) '())
  (check-equal? (for/list ([x (in-amb/do (for/amb #:length 5 ([i '()]) i) #:empty-handler P)]) x) '(P))
  (check-equal? (for/list ([x (in-amb/do (for/amb #:length 0 ([i '(1 2)]) i) #:empty-handler P)]) x) '(P))
  ;; a partly filled vector, even shuffled, is not (amb)
  (parameterize ([current-amb-shuffler (λ (v) (vector-reverse! v))])
    (check-equal? (for/list ([x (in-amb/do (for/amb #:length 5 ([i '(1 2)]) i) #:empty-handler P)]) x)
                  '(2 1))))

(test-case "(amb) inside the handler is a recursive call; the innermost answer counts"
  ;; a counter makes it stop; the outer calls are abandoned
  (define n 0)
  (define calls '())
  (check-equal? (stream->list
                 (in-amb (amb)
                         #:empty-handler
                         (λ ()
                           (set! n (add1 n))
                           (set! calls (cons n calls))
                           (if (< n 3) (list 'outer (amb)) n))))
                '(3))
  (check-equal? (reverse calls) '(1 2 3))
  ;; other choices inside the handler are fine
  (check-equal? (stream->list (in-amb (amb) #:empty-handler (λ () (amb 1 2)))) '(1 2))
  ;; an inner search inside the handler has its own handler
  (check-equal? (stream->list (in-amb (amb) #:empty-handler (λ () (stream->list (in-amb (amb 1 (amb)))))))
                '((1))))

(test-case "the recursion goes through empty choices and resumed continuations too"
  (for ([h (list (λ () (for/amb ([i '()]) i))
                 (λ () (for/amb #:length 0 ([i '()]) i))
                 (λ () (make-amb (λ () #f) void))
                 (λ () (when (amb #t #f) (amb)) 'P))])
    (define n 0)
    (define (bounded) (set! n (add1 n)) (if (> n 3) 'done (h)))
    (check-equal? (for/list ([x (in-amb/do (amb) #:empty-handler bounded)] #:unless (eq? x 'P)) x)
                  '(done))
    (check-equal? n 4)))

(test-case "a handler can avoid the recursion with a frontier of its own"
  (define (handler) (parameterize ([current-amb-frontier (make-amb-frontier (λ () 'inner))])
                      (list 'outer (amb))))
  (check-equal? (stream->list (in-amb (amb 1 (amb) 2) #:empty-handler handler))
                '(1 inner 2)))

(test-case "an inner search without a handler, resumed lazily across outer choices"
  (check-equal?
   (stream->list
    (in-amb (let ([s (in-amb (begin (suspend) (amb 1 (for/amb ([i '()]) i) 2)))])
              (suspend)
              (let ([a (stream-first s)]) (suspend) (list a (stream->list (stream-rest s)))))
            #:empty-handler P))
   '(P P (1 (2))))
  ;; created outside, resumed inside a search with a handler
  (define-values (more? get)
    (sequence-generate (in-amb/do (begin (suspend) (make-amb (λ () #f) void) 'never))))
  (check-equal? (stream->list (in-amb (list (more?)) #:empty-handler P)) '((#f))))

(test-case "three levels: with, without, with"
  (check-equal?
   (stream->list
    (in-amb (stream->list
             (in-amb (list (amb 'm (amb))
                           (stream->list (in-amb (amb 'x (amb)) #:empty-handler (λ () 'IN))))))
            #:empty-handler (λ () 'OUT)))
   '(((m (x IN))))))

(test-case "an empty for/amb does not change the order of side effects (every strategy)"
  (for ([df   '(#t #t #f #f)]
        [fair '(#t #f #t #f)]
        [expected-log '((probe open choose) (probe open choose)
                        (open probe choose) (open choose probe))])
    (define log '())
    (parameterize ([current-amb-depth-first? df] [current-amb-fair? fair])
      (stream->list
       (in-amb
        (amb (for/amb ([i (begin (set! log (cons 'probe log)) '())]) i)
             (begin (set! log (cons 'open log)) 's)
             (begin (set! log (cons 'choose log)) (amb 1 2))))))
    (check-equal? (reverse log) expected-log (format "df=~a fair=~a" df fair))))

(test-case "after the handler raises, in-amb/do can go on"
  (define-values (more? get)
    (sequence-generate (in-amb/do (amb 1 (amb) 2) #:empty-handler (λ () (error "boom")))))
  (check-equal? (get) 1)
  (check-exn #rx"boom" get)
  (check-equal? (get) 2))

(test-case "another prompt between (amb) and the search does not hide the handler"
  (check-equal? (stream->list (in-amb (call-with-continuation-prompt (λ () (+ 1 (amb 1 (amb)))))
                                      #:empty-handler (λ () 'P)))
                '(2 P)))

(test-case "(amb) in a for/amb body"
  (check-equal? (stream->list (in-amb (for/amb ([i '(1 2 3)]) (if (odd? i) (amb) i)) #:empty-handler P))
                '(P 2 P)))

(test-case "an inner search without a handler, consumed whole inside an outer one, still fails at (amb)"
  (check-equal? (stream->list (in-amb (stream->list (in-amb (amb 1 (amb) 2))) #:empty-handler P))
                '((1 2))))

(test-case "the handler sees the parameterization of whoever resumes the search"
  (define p (make-parameter 'creation))
  (define-values (more? get)
    (sequence-generate (in-amb/do (begin (suspend) (suspend) 'x) #:empty-handler p)))
  (check-equal? (list (get) (parameterize ([p 'later]) (get)) (get)) '(creation later x)))

(test-case "the handler must be a thunk, in every form"
  (check-exn exn:fail:contract? (λ () (in-amb (amb) #:empty-handler (λ (x) x))))
  (check-exn exn:fail:contract? (λ () (for/list ([x (in-amb (amb) #:empty-handler 'P)]) x)))
  (check-exn exn:fail:contract? (λ () (in-amb/do (amb) #:empty-handler 'P)))
  (check-exn exn:fail:contract? (λ () (for/list ([x (in-amb/do (amb) #:empty-handler 'P)]) x)))
  (check-exn exn:fail:contract? (λ () (in-amb*/do (λ () 1) 'P))))

(test-case "outside any search they are errors"
  (check-exn exn:fail? (λ () (amb)))
  (check-exn exn:fail? (λ () (amb-fail))))
