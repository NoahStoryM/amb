#lang racket/base

;; Pauses and the `#:on-pause` handler.  Run with: raco test pkgs/amb/tests/amb-handler.rkt
;;
;; Ported from the v2 tests of `amb-fail` and the `#:empty-handler`
;; handler for `(amb)`.  In v2 a pause was written (when (amb #t #f)
;; (amb)), and `(amb)` was answered by the empty handler of the search.
;; In v3 `(amb)` always fails, a pause is `(amb-pause)`, and the handler
;; of `#:on-pause` is called by the observer of the search.
;;
;; Every test case of v2 is here under its v2 name.  A case whose v2
;; expectation no longer holds is marked CONFLICT, checks what v3 does,
;; and is listed with its v2 expectation in v2-conflicts.rkt.
(require rackunit racket/sequence racket/stream amb amb/merge)

(define suspend amb-pause)
(define (P) 'P)
(define (vector-reverse! v)
  (define n (vector-length v))
  (for ([i (quotient n 2)])
    (define t (vector-ref v i))
    (vector-set! v i (vector-ref v (- n i 1)))
    (vector-set! v (- n i 1) t)))
(define mergers (list amb-append amb-interleave amb-alternate amb-level))

(test-case "by default (amb) is (amb-fail)"
  (check-equal? (for/list ([x (in-amb/do (amb (amb-fail) 1 (amb) 2))]) x) '(1 2))
  (check-equal? (stream->list (in-amb (amb (amb) 5))) '(5))
  ;; so a pause only reschedules: nothing is lost or added
  (check-equal? (for/list ([x (in-amb/do (begin (suspend) (amb 1 (begin (suspend) 2))))]) x)
                '(1 2)))

;; CONFLICT: (amb) is not answered by a handler.
(test-case "with #:empty-handler, (amb) outputs the handler's values and abandons the rest"
  (check-equal? (for/list ([x (in-amb/do (begin (amb) 'late) #:on-pause P)]) x) '())
  (check-equal? (stream->list (in-amb (let ([x (amb 1 2)]) (amb) x) #:on-pause P)) '()))

(test-case "(amb-fail) is never answered by the handler"
  (check-equal? (for/list ([x (in-amb/do (amb (amb-fail) 1) #:on-pause P)]) x) '(1)))

;; CONFLICT: a choice with no alternatives fails.
(test-case "a choice with no alternatives is (amb)"
  (check-equal? (for/list ([x (in-amb/do (for/amb ([i '()]) i) #:on-pause P)]) x) '())
  (check-equal? (for/list ([x (in-amb/do (for/amb #:length 0 ([i '()]) i) #:on-pause P)]) x) '())
  (check-equal? (for/list ([x (in-amb/do (make-amb (λ () (values #f void))) #:on-pause P)]) x) '())
  ;; running out after some alternatives is the same
  (check-equal? (for/list ([x (in-amb/do (for/amb ([i '(1 2)]) i) #:on-pause P)]) x) '(1 2))
  (check-equal? (for/list ([x (in-amb/do (amb 1 2) #:on-pause P)]) x) '(1 2)))

;; CONFLICT: the handler runs where the search is observed.
(test-case "the handler runs where (amb) is called"
  (define p (make-parameter 'outside))
  (check-equal? (for/list ([x (in-amb/do (parameterize ([p 'inside]) (amb-pause) 'x) #:on-pause p)]) x)
                '(outside x))
  ;; it may raise
  (check-exn #rx"boom" (λ () (for/list ([x (in-amb/do (amb-pause) #:on-pause (λ () (error "boom")))]) x))))

(test-case "function forms take the handler as an optional argument"
  (check-equal? (for/list ([x (in-amb*/do (λ () (suspend) 'x) P)]) x) '(P x))
  (check-equal? (stream->list (in-amb* (λ () (suspend) 'x) P)) '(P x))
  (check-equal? (for/list ([x (in-amb* (λ () (suspend) 'x) P)]) x) '(P x))
  (check-equal? (for/list ([x (in-amb (begin (suspend) 'x) #:on-pause P)]) x) '(P x))
  (check-equal? (for/list ([x (in-amb*/do (λ () (suspend) 'x) #f)]) x) '(x)))

(test-case "zero and several values"
  (check-equal? (for/list ([(a b) (in-amb/do (begin (suspend) (values 3 4)) #:on-pause (λ () (values 1 2)))])
                  (list a b))
                '((1 2) (3 4)))
  (check-equal? (for/list ([() (in-amb/do (begin (suspend) (values)) #:on-pause values)]) 'tick)
                '(tick tick)))

(test-case "the pause idiom: output, then continue after it"
  (define log '())
  (define out
    (for/list ([x (in-amb/do (begin (set! log (cons 'before log))
                                    (suspend)
                                    (set! log (cons 'after log))
                                    'x)
                             #:on-pause P)])
      (set! log (cons (list 'got x) log))
      x))
  (check-equal? out '(P x))
  (check-equal? (reverse log) '(before (got P) after (got x))))

(test-case "each search uses its own handler; an inner search without one hides pauses"
  (check-equal?
   (for/list ([x (in-amb/do
                  (let-values ([(more? get)
                                (sequence-generate
                                 (in-amb/do (begin (suspend) 'inner) #:on-pause (λ () 'inner-P)))])
                    (suspend)
                    (list (get) (get)))
                  #:on-pause (λ () 'outer-P))])
     x)
   '(outer-P (inner-P inner)))
  ;; a plain inner search (no handler) skips its pauses: the answer survives
  (check-equal?
   (for/list ([x (in-amb/do
                  (let-values ([(more? get)
                                (sequence-generate (in-amb/do (begin (suspend) 'inner)))])
                    (list (and (more?) (get))))
                  #:on-pause (λ () 'outer-P))])
     x)
   '((inner))))

(test-case "the handler is the search's, wherever its trees are resumed"
  (define-values (more? get)
    (sequence-generate (in-amb/do (begin (suspend) (suspend) 'x) #:on-pause P)))
  ;; resumed from inside another search that has a different handler
  (check-equal? (for/list ([_ 3])
                  (for/first ([v (in-amb/do (get) #:on-pause (λ () 'other))]) v))
                '(P P x)))

;; CONFLICT (strategy parameters): the four mergers instead.
(test-case "answer sets are unchanged under every strategy"
  (for ([m mergers])
    (define vs
      (parameterize ([current-amb-merger m])
        (for/list ([v (in-amb/do (let ([x (amb 1 2 3)]) (suspend) x) #:on-pause P)]
                   #:unless (eq? v 'P))
          v)))
    (check-equal? (sort vs <) '(1 2 3) (format "~a" (object-name m)))))

(test-case "a computation that keeps pausing yields control each time"
  (define (spin) (suspend) (spin))
  (define-values (more? get) (sequence-generate (in-amb/do (spin) #:on-pause P)))
  (check-equal? (for/list ([_ 5]) (and (more?) (get))) '(P P P P P)))

(test-case "the handler must be a thunk"
  (check-exn exn:fail:contract? (λ () (in-amb* (λ () 1) 'P)))
  (check-exn exn:fail:contract? (λ () (for/list ([x (in-amb* (λ () 1) 'P)]) x))))

;; CONFLICT: the shuffler is called once for every vector of
;; alternatives, the empty one too.
(test-case "a choice with no alternative does not call the shuffler"
  (define calls 0)
  (parameterize ([current-amb-shuffler
                  (λ (v) (when (zero? (vector-length v)) (set! calls (add1 calls))))])
    (check-equal? (for/list ([x (in-amb/do (amb))]) x) '())
    (check-equal? (for/list ([x (in-amb/do (for/amb #:length 0 ([i '()]) i))]) x) '())
    (check-equal? (stream->list (in-amb (amb (amb) calls))) '(3)))
  (check-equal? calls 3))

(test-case "an empty for/amb evaluates its clauses once, with or without a handler"
  (define n 0)
  (check-equal? (for/list ([x (in-amb/do (for/amb ([i (begin (set! n (add1 n)) '())]) i))]) x) '())
  (check-equal? n 1)
  (set! n 0)
  (check-equal? (for/list ([x (in-amb/do (for*/amb ([i (begin (set! n (add1 n)) '())]) i) #:on-pause P)]) x)
                '())
  (check-equal? n 1)
  ;; a second evaluation would not be empty: it must not happen
  (define src '())
  (check-equal? (for/list ([x (in-amb/do (for/amb ([i (begin0 src (set! src '(1 2)))]) i))]) x) '())
  (set! src '())
  (check-equal? (for/list ([x (in-amb/do (for/amb ([i (begin0 src (set! src '(1 2)))]) i) #:on-pause P)]) x)
                '()))

;; CONFLICT (make-amb): one generator instead of `more?` and `get`.
(test-case "make-amb does not call more? again after it returned #f"
  (define m 0)
  (check-equal? (for/list ([x (in-amb/do (make-amb (λ () (set! m (add1 m)) (values #f void))))]) x) '())
  (check-equal? m 1)
  ;; the generator that comes with #f is not called
  (check-equal? (stream->list (in-amb (make-amb (λ () (values #f (λ () (error "called after the end")))))))
                '()))

;; CONFLICT (strategy parameters): the four mergers instead.  The order
;; of the side effects is the same for all four: nothing pauses.
(test-case "an empty generator does not change the order of side effects (every strategy)"
  (for ([m mergers])
    (define open? #f)
    (define rev? #f)
    (define log '())
    (define result
      (parameterize ([current-amb-merger m])
        (stream->list
         (in-amb
          (amb (make-amb (λ () (set! log (cons 'probe log)) (when open? (set! rev? #t)) (values #f void)))
               (begin (set! log (cons 'open log)) (set! open? #t) 's)
               (begin (set! log (cons 'choose log)) (if rev? (amb 2 1) (amb 1 2))))))))
    (check-equal? result '(s 1 2) (format "~a" (object-name m)))
    (check-equal? (reverse log) '(probe open choose) (format "~a" (object-name m)))))

;; CONFLICT: no vector of alternatives is answered by a handler.
(test-case "#:length: only the default fill counts as no alternative"
  (check-equal? (for/list ([x (in-amb/do (for/amb #:length 2 #:fill 0 ([i '()]) i) #:on-pause P)]) x) '(0 0))
  (check-equal? (for/list ([x (in-amb/do (for/amb #:length 2 #:fill (amb-fail) ([i '()]) i) #:on-pause P)]) x) '())
  (check-equal? (for/list ([x (in-amb/do (for/amb #:length 5 ([i '()]) i) #:on-pause P)]) x) '())
  (check-equal? (for/list ([x (in-amb/do (for/amb #:length 0 ([i '(1 2)]) i) #:on-pause P)]) x) '())
  ;; a partly filled vector is shuffled without the slots that were not filled
  (parameterize ([current-amb-shuffler (λ (v) (vector-reverse! v))])
    (check-equal? (for/list ([x (in-amb/do (for/amb #:length 5 ([i '(1 2)]) i) #:on-pause P)]) x)
                  '(2 1))))

;; CONFLICT: there is no empty handler, and so no reentry into it.  The
;; pause handler runs outside the search: an effect in it belongs to
;; whatever search observes this one.
(test-case "(amb) inside the handler of the same search is an error; other choices are fine"
  (check-exn exn:fail:contract? (λ () (stream->list (in-amb (amb-pause) #:on-pause (λ () (amb))))))
  (check-equal? (stream->list (in-amb (stream->list (in-amb (amb-pause) #:on-pause (λ () (amb 1 2))))))
                (list (list 1 (void)) (list 2 (void))))
  ;; an inner search inside the handler
  (check-equal? (stream->list (in-amb (amb-pause) #:on-pause (λ () (stream->list (in-amb (amb 1 (amb)))))))
                (list '(1) (void))))

;; CONFLICT: removed with the empty handler.
(test-case "reentry is detected through empty choices and resumed continuations too"
  (void))

;; CONFLICT: removed with the empty handler.
(test-case "after a reentry error the search goes on"
  (void))

(test-case "an inner search without a handler, resumed lazily across outer choices"
  (check-equal?
   (stream->list
    (in-amb (let ([s (in-amb (begin (suspend) (amb 1 (for/amb ([i '()]) i) 2)))])
              (suspend)
              (let ([a (stream-first s)]) (suspend) (list a (stream->list (stream-rest s)))))
            #:on-pause P))
   '(P P (1 (2))))
  ;; created outside, resumed inside a search with a handler
  (define-values (more? get)
    (sequence-generate (in-amb/do (begin (suspend) (make-amb (λ () (values #f void))) 'never))))
  (check-equal? (stream->list (in-amb (list (more?)) #:on-pause P)) '((#f))))

;; CONFLICT: (amb) is not answered by a handler; with pauses instead.
(test-case "three levels: with, without, with"
  (check-equal?
   (stream->list
    (in-amb (stream->list
             (in-amb (list (amb 'm (amb-pause))
                           (stream->list (in-amb (amb 'x (begin (amb-pause) (amb))) #:on-pause (λ () 'IN))))))
            #:on-pause (λ () 'OUT)))
   (list (list '(m (x IN)) (list (void) '(x IN))))))

;; CONFLICT (strategy parameters): the four mergers instead.
(test-case "an empty for/amb does not change the order of side effects (every strategy)"
  (for ([m mergers])
    (define log '())
    (parameterize ([current-amb-merger m])
      (stream->list
       (in-amb
        (amb (for/amb ([i (begin (set! log (cons 'probe log)) '())]) i)
             (begin (set! log (cons 'open log)) 's)
             (begin (set! log (cons 'choose log)) (amb 1 2))))))
    (check-equal? (reverse log) '(probe open choose) (format "~a" (object-name m)))))

(test-case "after the handler raises, in-amb/do can go on"
  (define-values (more? get)
    (sequence-generate (in-amb/do (amb 1 (begin (amb-pause) (amb)) 2) #:on-pause (λ () (error "boom")))))
  (check-equal? (get) 1)
  (check-exn #rx"boom" get)
  (check-equal? (get) 2))

;; CONFLICT: a prompt of another tag between an effect and the search
;; is fine, as in v2; the default prompt tag is another matter and is
;; not tested here.
(test-case "another prompt between (amb) and the search does not hide the handler"
  (define tag (make-continuation-prompt-tag))
  (check-equal? (stream->list (in-amb (call-with-continuation-prompt
                                       (λ () (+ 1 (amb 1 (begin (amb-pause) 10))))
                                       tag)
                                      #:on-pause (λ () 'P)))
                '(2 P 11)))

;; CONFLICT: (amb) in a body fails.
(test-case "(amb) in a for/amb body"
  (check-equal? (stream->list (in-amb (for/amb ([i '(1 2 3)]) (if (odd? i) (amb) i)) #:on-pause P))
                '(2))
  (check-equal? (stream->list (in-amb (for/amb ([i '(1 2 3)]) (if (odd? i) (begin (amb-pause) i) i)) #:on-pause P))
                '(P 1 2 P 3)))

(test-case "an inner search without a handler, consumed whole inside an outer one, still fails at (amb)"
  (check-equal? (stream->list (in-amb (stream->list (in-amb (amb 1 (amb) 2))) #:on-pause P))
                '((1 2))))

(test-case "the handler sees the parameterization of whoever resumes the search"
  (define p (make-parameter 'creation))
  (define-values (more? get)
    (sequence-generate (in-amb/do (begin (suspend) (suspend) 'x) #:on-pause p)))
  (check-equal? (list (get) (parameterize ([p 'later]) (get)) (get)) '(creation later x)))

(test-case "the handler must be a thunk, in every form"
  (check-exn exn:fail:contract? (λ () (in-amb (amb) #:on-pause (λ (x) x))))
  (check-exn exn:fail:contract? (λ () (for/list ([x (in-amb (amb) #:on-pause 'P)]) x)))
  (check-exn exn:fail:contract? (λ () (in-amb/do (amb) #:on-pause 'P)))
  (check-exn exn:fail:contract? (λ () (for/list ([x (in-amb/do (amb) #:on-pause 'P)]) x)))
  (check-exn exn:fail:contract? (λ () (in-amb*/do (λ () 1) 'P))))

(test-case "outside any search they are errors"
  (check-exn exn:fail? (λ () (amb)))
  (check-exn exn:fail? (λ () (amb-fail)))
  (check-exn exn:fail? (λ () (amb-pause))))
