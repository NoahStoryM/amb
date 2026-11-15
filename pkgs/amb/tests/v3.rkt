#lang racket/base

;; What is new in v3: the generator protocol, the mergers as values,
;; `amb-pause`, `amb-generate*`, and `#:on-pause`.  The section numbers
;; are those of V3-PLAN.md.

(require rackunit
         racket/stream
         amb
         amb/merge
         (prefix-in base: amb/base))

(define mergers (list amb-append amb-interleave amb-alternate amb-level))
(define-syntax-rule (events e)
  (for/list ([x (in-amb/do e #:on-pause (λ () 'P))]) x))
(define-syntax-rule (with m e ...) (parameterize ([current-amb-merger m]) e ...))

;; a generator from its raw results
(define (source . rs)
  (λ ()
    (if (null? rs)
        (values #f (λ () (error "called after the end")))
        (values (car rs) (apply source (cdr rs))))))
;; i, after n pauses
(define (later n i) (for ([_ n]) (amb-pause)) i)


;; ---- 1. modules ----

(test-case "amb/main and amb/base export the same bindings"
  (check-eq? make-amb base:make-amb)
  (check-eq? amb-generate* base:amb-generate*)
  (check-eq? amb-fail base:amb-fail)
  (check-eq? amb-pause base:amb-pause)
  (check-eq? current-amb-merger base:current-amb-merger)
  (check-eq? (current-amb-merger) amb-append))

(test-case "the parameters check their values"
  (check-exn exn:fail:contract? (λ () (current-amb-merger 'append)))
  (check-exn exn:fail:contract? (λ () (current-amb-merger (λ (s) s))))
  (check-exn exn:fail:contract? (λ () (current-amb-shuffler 'shuffle)))
  (check-exn exn:fail:contract? (λ () (make-amb 1)))
  (check-exn exn:fail:contract? (λ () (amb-generate* 1)))
  (check-exn exn:fail:contract? (λ () (sequence->amb 1.5))))


;; ---- 2. the generator protocol ----

(test-case "a list is an answer, n is n pauses, #f is the end"
  (check-equal? (events (make-amb (source '(a) 2 '(b) 1))) '(a P P b P))
  ;; '() is an answer of no values, not the end
  (check-equal? (for/list ([() (in-amb/do (make-amb (source '() '())))]) 'answer)
                '(answer answer))
  (check-equal? (for/list ([(a b) (in-amb/do (make-amb (source '(1 2) '(3 4))))]) (list a b))
                '((1 2) (3 4)))
  ;; what `sequence-generate*` returns is a generator
  (check-equal? (events (make-amb (λ () (sequence-generate* '(1 2 3))))) '(1 2 3)))

(test-case "0 is not a result"
  (define (zero) (values 0 zero))
  (for ([m mergers])
    (check-exn #rx"expected: \\(or/c list\\? exact-positive-integer\\? #f\\)\n  given: 0"
               (λ () ((m zero (source)))))
    ;; from the second generator too; it is the merger or, when the
    ;; second generator goes on by itself, whoever calls it that raises
    (check-exn #rx"given: 0" (λ () (with m (events (make-amb (m (source 1) zero))))))
    (check-exn #rx"given: 'x" (λ () ((m (source 'x) (source))))))
  (check-exn #rx"make-amb: contract violation.*given: 0"
             (λ () (events (make-amb zero))))
  (check-exn #rx"in-amb: contract violation.*given: 0"
             (λ () (events (with (λ (s t) zero) (amb 1 2))))))


;; ---- 4. mergers ----

;; every merge starts with a pause, then is amb-append
(define ((tick-append s t)) (values 1 (amb-append s t)))

(test-case "the choice is folded from the right, whatever the merger"
  ;; μ(a, μ(b, end)): the end of the supply is merged too
  (with tick-append
    (check-equal? (events (amb 'a 'b)) '(P a P b))
    (check-equal? (events (amb 'a)) '(P a))
    ;; (amb) is the end of the supply at once: no merge
    (check-equal? (events (amb)) '())
    (check-equal? (events (amb (amb) (amb))) '(P P))
    (check-equal? (events (sequence->amb '(a b))) '(P a P b))
    (check-equal? (events (for/amb ([x '(a b)]) x)) '(P a P b))
    (check-equal? (events (for/amb #:length 2 ([x '(a b)]) x)) '(P a P b))))

(test-case "the pauses of the supply are pauses of the choice, where they are"
  (for ([m mergers])
    (with m
      (check-equal? (events (make-amb (source 2 '(a) 1 '(b)))) '(P P a P b))))
  (with tick-append
    (check-equal? (events (make-amb (source '(a) 2 '(b)))) '(P a P P P b))))

(test-case "the merger is read where the choice is made"
  (check-equal? (events (list (with amb-alternate (amb 1 2)) (amb 'a 'b)))
                '((1 a) (2 a) (1 b) (2 b)))
  (check-equal? (with amb-alternate (events (list (with amb-append (amb 1 2)) (amb 'a 'b))))
                '((1 a) (1 b) (2 a) (2 b))))

(test-case "the four mergers"
  (define-syntax-rule (three) (amb (later 2 'a) (amb (later 0 'b) (later 1 'c))))
  (check-equal? (with amb-append (events (three))) '(P P a b P c))
  (check-equal? (with amb-interleave (events (three))) '(P b P P c a))
  (check-equal? (with amb-alternate (events (three))) '(P b P P a c))
  (check-equal? (with amb-level (events (three))) '(b P c P a)))


;; ---- 7. choices ----

(test-case "the shuffler is called once for every vector of alternatives"
  (define sizes '())
  (define (spy v)
    (set! sizes (cons (vector-length v) sizes))
    (define n (vector-length v))
    (for ([i (quotient n 2)])
      (define x (vector-ref v i))
      (vector-set! v i (vector-ref v (- n i 1)))
      (vector-set! v (- n i 1) x)))
  (parameterize ([current-amb-shuffler spy])
    (check-equal? (events (amb 1 2 3)) '(3 2 1))
    (check-equal? (events (amb 1)) '(1))
    (check-equal? (events (amb)) '())
    (check-equal? (events (for/amb #:length 5 ([i 3]) i)) '(2 1 0))
    (check-equal? (events (for/amb #:length 0 ([i 3]) i)) '())
    (check-equal? (reverse sizes) '(3 1 0 3 0))
    ;; not for the alternatives of a sequence
    (set! sizes '())
    (check-equal? (events (for/amb ([i 3]) i)) '(0 1 2))
    (check-equal? (events (sequence->amb '(1 2 3))) '(1 2 3))
    (check-equal? sizes '())))

(test-case "no alternative is evaluated before it is chosen"
  (check-equal? (for/first ([x (in-amb/do (amb 1 (error "second")))]) x) 1)
  (check-equal? (for/first ([x (in-amb/do (for/amb ([i (in-naturals)]) (if (zero? i) i (error "second"))))]) x) 0)
  (check-equal? (for/first ([x (in-amb/do (for/amb #:length 2 ([i 2]) (if (zero? i) i (error "second"))))]) x) 0))


;; ---- 8. amb-generate* ----

(test-case "amb-generate* runs a search of its own, up to its first event"
  (define log '())
  (define (note x) (set! log (cons x log)) x)
  (define-values (v next) (amb-generate* (λ () (note 'start) (amb (note 1) (begin (amb-pause) (note 2))))))
  ;; eager: it has run up to the first answer
  (check-equal? (list v (reverse log)) '((1) (start 1)))
  (define-values (v2 next2) (next))
  (check-equal? (list v2 (reverse log)) '(1 (start 1)))
  (define-values (v3 next3) (next2))
  (check-equal? (list v3 (reverse log)) '((2) (start 1 2)))
  (define-values (v4 next4) (next3))
  (check-false v4)
  ;; no effect of the thunk reaches the search around it
  (check-equal? (events (let-values ([(v next) (amb-generate* (λ () (amb-pause) (amb 1 2)))]) v))
                '(1)))

;; the local search of e: its answers, searched for on their own
(define-syntax-rule (amb-local e)
  (make-amb (λ () (amb-generate* (λ () e)))))
;; the first answer of (thk), or failure; its pauses are passed on
(define (once thk)
  (let loop ([gen (λ () (amb-generate* thk))])
    (define-values (v next) (gen))
    (cond [(list? v) (apply values v)]
          [(not v) (amb-fail)]
          [else (for ([_ v]) (amb-pause)) (loop next)])))

(test-case "a local search is make-amb of amb-generate*"
  ;; with nothing after it, the same events for the standard mergers
  (for ([m mergers])
    (with m
      (check-equal? (events (amb-local (amb (later 1 'a) (amb 'b (later 2 'c)))))
                    (events (amb (later 1 'a) (amb 'b (later 2 'c)))))))
  ;; but not for every merger: each answer is one more merge
  (with tick-append
    (check-equal? (events (amb 'a 'b)) '(P a P b))
    (check-equal? (events (amb-local (amb 'a 'b))) '(P P a P P b)))
  ;; with something after it, the choices of the local search are merged
  ;; with each other first
  (with amb-level
    (check-equal? (events (list (amb (later 1 1) 2) (amb 'x (later 1 'y))))
                  '((2 x) P (1 x) (2 y) P (1 y)))
    (check-equal? (events (list (amb-local (amb (later 1 1) 2)) (amb 'x (later 1 'y))))
                  '((2 x) P (2 y) (1 x) P (1 y)))))

(test-case "once, written with amb-generate*, amb-pause and amb-fail"
  (check-equal? (events (once (λ () (amb 1 2 3)))) '(1))
  (check-equal? (events (once (λ () (amb)))) '())
  (with amb-interleave
    ;; the pauses of the test are pauses of the search around it
    (check-equal? (events (amb (once (λ () (amb (later 2 'a) 'b))) 'x)) '(P x b))))


;; ---- 10. observers ----

(test-case "nothing runs before the first element is asked for"
  (define ran? #f)
  (define s (in-amb (begin (set! ran? #t) 1)))
  (define d (in-amb/do (begin (set! ran? #t) 1)))
  (define s* (in-amb* (λ () (set! ran? #t) 1)))
  (define d* (in-amb*/do (λ () (set! ran? #t) 1)))
  (check-false ran?)
  (check-equal? (stream-first s) 1)
  (check-true ran?))

(test-case "in-amb remembers its elements, the handler's too"
  (define runs 0)
  (define calls 0)
  (define s (in-amb (begin (set! runs (add1 runs)) (amb-pause) (amb 1 2))
                    #:on-pause (λ () (set! calls (add1 calls)) 'P)))
  (check-equal? (stream->list s) '(P 1 2))
  (check-equal? (stream->list s) '(P 1 2))
  (check-equal? (for/list ([x (in-stream s)]) x) '(P 1 2))
  (check-equal? (list runs calls) '(1 1)))

(test-case "the handler is called once for each of n pauses"
  (define calls 0)
  (check-equal? (for/list ([x (in-amb/do (make-amb (source 3 '(a) 2))
                                         #:on-pause (λ () (set! calls (add1 calls)) calls))])
                  x)
                '(1 2 3 a 4 5))
  ;; without a handler, pauses are skipped
  (check-equal? (for/list ([x (in-amb/do (make-amb (source 3 '(a) 2)))]) x) '(a)))

(test-case "in-amb/do is single-pass"
  (define d (in-amb/do (amb 1 2 3)))
  (check-equal? (for/first ([x d]) x) 1)
  (check-equal? (for/list ([x d]) x) '(2 3))
  (check-equal? (for/list ([x d]) x) '()))

(test-case "a search that raised cannot go on"
  ;; what it has run is not run again
  (define runs 0)
  (define-values (more? get)
    (sequence-generate
     (in-amb/do (amb 1 (begin (set! runs (add1 runs)) (error "boom")) 3))))
  (check-equal? (get) 1)
  (check-exn #rx"boom" get)
  (check-exn #rx"the search cannot go on" get)
  (check-exn #rx"the search cannot go on" more?)
  (check-equal? runs 1)
  ;; the stream too
  (define s (in-amb (amb 1 (begin (set! runs (add1 runs)) (error "boom")) 3)))
  (check-equal? (stream-first s) 1)
  (check-exn #rx"boom" (λ () (stream-first (stream-rest s))))
  ;; (the stream itself refuses to be forced again)
  (check-exn exn:fail? (λ () (stream-first (stream-rest s))))
  (check-equal? runs 2)
  ;; a handler that raises is another matter: the pause is over
  (define-values (more?* get*)
    (sequence-generate
     (in-amb/do (begin (amb-pause) 'x) #:on-pause (λ () (error "handler")))))
  (check-exn #rx"handler" get*)
  (check-equal? (get*) 'x))

(test-case "an effect in a merger is not an effect of the search that it merges"
  ;; The merger pauses before it calls its generators.  The pause is an
  ;; effect of whatever search the merged generator is called in, and
  ;; that is not a guarantee of anything: the first merge of a choice is
  ;; called where the choice was made, in the search; the others are
  ;; called by whoever asks for the next event.
  (define ((pausing s t)) (amb-pause) ((amb-append s t)))
  ;; (amb 1 2) is two merges.  The observer is not in a search:
  (check-exn #rx"amb-pause: not in a search"
             (λ () (with pausing (events (amb 1 2)))))
  ;; it is in a search: that one pauses for the second merge
  (check-equal? (events (with pausing (events (amb 1 2)))) '(P (P 1 2)))
  ;; a supplier
  (check-exn #rx"amb-pause: not allowed while the alternatives of a choice are supplied"
             (λ () (events (sequence->amb (in-amb/do (with pausing (amb 1 2))))))))
