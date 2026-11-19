#lang racket/base

;; What is new in v3: the generator protocol, the mergers as values,
;; `amb-pause`, `amb-generate*`, and `#:on-pause`.  The section numbers
;; are those of V3-PLAN.md.

(require rackunit
         racket/list
         racket/stream
         amb
         amb/merge
         (prefix-in base: amb/base)
         (prefix-in choice: "../private/choice.rkt"))

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
  (check-eq? call-with-amb-mark base:call-with-amb-mark)
  (check-eq? (current-amb-merger) amb-append))

(test-case "choice runtime is shared without exporting its helpers"
  (check-eq? sequence->amb choice:sequence->amb)
  (check-eq? amb-local* choice:amb-local*)
  (check-eq? current-amb-shuffler choice:current-amb-shuffler)
  (for* ([module '(amb amb/base)]
         [name '(thunks choose choose-none make-choice-generator
                 mutable-vector-generate*)])
    (check-exn exn:fail? (λ () (dynamic-require module name)))))

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

(test-case "amb-pause emits the count in one generator step and resumes with void"
  (define-values (v next) (amb-generate* (λ () (void? (amb-pause 3)))))
  (define-values (answer _) (next))
  (check-equal? v 3)
  (check-equal? answer '(#t))
  (check-equal? (events (begin (amb-pause 1) 'answer))
                (events (begin (amb-pause) 'answer))))

(test-case "amb-pause validates an explicit count and checks the search context"
  (for ([n '(0.0 -1 1.0 1/2 #f)])
    (check-exn #rx"amb-pause: contract violation.*exact-nonnegative-integer"
               (λ () (amb-generate* (λ () (amb-pause n))))))
  (check-exn #rx"amb-pause: not in a search" (λ () (amb-pause 3)))
  (check-exn #rx"amb-pause: not allowed while the alternatives of a choice are supplied"
             (λ () (amb-generate*
                    (λ () (make-amb (λ () (amb-pause 3))))))))

(test-case "amb-pause with zero returns void without reporting an event"
  (check-true (void? (amb-pause 0)))
  (define-values (v _)
    (amb-generate* (λ () (amb-pause 0) 'answer)))
  (check-equal? v '(answer))
  (check-equal? (events (make-amb (λ () (amb-pause 0) (values '(answer) (source)))))
                '(answer)))

(test-case "an observer demands handled pauses one at a time"
  (define calls 0)
  (check-equal?
   (for/first ([v (in-amb/do (begin (amb-pause (expt 2 80)) (error "resumed too soon"))
                              #:on-pause (λ () (set! calls (add1 calls)) 'P))])
     v)
   'P)
  (check-equal? calls 1))


;; ---- 4. mergers ----

(test-case "the standard mergers schedule a batch as consecutive logical pauses"
  (for ([m mergers])
    (check-equal? (with m (events (amb (begin (amb-pause 3) 'left) (later 2 'right))))
                  (with m (events (amb (later 3 'left) (later 2 'right)))))))

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


(test-case "an initial pause does not make an answer-only remainder interleave fairly"
  (define (search)
    (for/amb ([n (in-naturals)])
      (amb-pause)
      (if (zero? n) (for/amb ([i (in-naturals)]) 'a) n)))
  (define (prefix merge)
    (with merge
      (for/list ([_ (in-range 16)]
                 [v (in-amb/do (search) #:on-pause (λ () 'P))])
        v)))
  ;; A finite prefix illustrating the equation: interleave switches
  ;; only on a pause, whereas alternate also switches on an answer.
  (check-equal? (prefix amb-interleave) (append '(P P) (make-list 14 'a)))
  (define alternating (prefix amb-alternate))
  (for ([answer '(1 2 3)])
    (check-not-false (member answer alternating))))

(test-case "level finishes a finite round; supplier pauses can guard an infinite supply"
  (with amb-level
    (for ([width '(1 4 20)])
      (define started 0)
      (check-equal? (for/first ([v (in-amb/do
                                   (for/amb ([n (in-range width)])
                                     (set! started (add1 started))
                                     (amb-pause)
                                     n)
                                   #:on-pause (λ () 'P))])
                      v)
                    'P)
      (check-equal? started width))
    ;; The supplier itself pauses before each alternative.  This is
    ;; different from an unpaused infinite supply of paused bodies.
    (define ((supply n))
      (values 1 (λ () (values (list n) (supply (add1 n))))))
    (check-equal? (for/list ([_ (in-range 6)]
                           [v (in-amb/do (make-amb (supply 0)) #:on-pause (λ () 'P))])
                    v)
                  '(P 0 P 1 P 2))))


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
    (check-equal? (events (for/amb #:length 0 #:fill 9 ([i 3]) i)) '())
    (check-equal? (events (for*/amb #:length 0 ([i 3]) i)) '())
    (check-equal? (events (for/amb #:length 3 ([i 0]) i)) '())
    (check-equal? (reverse sizes) '(3 1 0 3 0 0 0 0))
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

;; the first answer of (thk), or failure; its pauses are passed on
(define (once thk)
  (let loop ([gen (λ () (amb-generate* thk))])
    (define-values (v next) (gen))
    (cond [(list? v) (apply values v)]
          [(not v) (amb-fail)]
          [else (for ([_ v]) (amb-pause)) (loop next)])))

(test-case "a local search is make-amb of amb-generate*"
  (for ([m mergers])
    (with m
      (check-equal? (events (list (amb-local (amb (later 1 1) 2)) (amb 'x (later 1 'y))))
                    (events (list (make-amb (λ () (amb-generate* (λ () (amb (later 1 1) 2)))))
                                  (amb 'x (later 1 'y)))))))
  ;; several values
  (check-equal? (for/list ([(a b) (in-amb/do (amb-local (amb (values 1 'x) (values 2 'y))))]) (list a b))
                '((1 x) (2 y)))
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

(test-case "amb-local* is the function behind amb-local"
  (define (local) (amb (later 2 'a) 'b))
  (for ([m mergers])
    (with m
      (check-equal? (events (list (amb-local* local) (amb 'x 'y)))
                    (events (list (amb-local (local)) (amb 'x 'y))))))
  (check-equal? (events (amb-local* amb-fail)) '())
  (check-equal? (for/list ([() (in-amb/do (amb-local* (λ () (values))))]) 'zero)
                '(zero))
  (check-equal? (for/list ([(a b) (in-amb/do (amb-local* (λ () (values 1 'x))))]) (list a b))
                '((1 x)))
  (define ran? #f)
  (define seq (in-amb*/do (λ () (amb-local* (λ () (set! ran? #t) 42)))))
  (check-false ran?)
  (check-equal? (for/list ([v seq]) v) '(42))
  (check-true ran?)
  (check-exn #rx"amb-local\\*: contract violation" (λ () (amb-local* 42)))
  (check-exn #rx"not allowed while" (λ () (events (with-amb-mark (amb-local* local)))))
  ;; Local pauses keep giving the enclosing search its turn.
  (define (never) (amb-pause) (never))
  (with amb-interleave
    (let loop ([g (λ () (amb-generate* (λ () (amb (amb-local* never) 'x))))]
               [fuel 8] [seen '()])
      (if (zero? fuel)
          (check-equal? (reverse seen) '(1 (x) 1 1 1 1 1 1))
          (let-values ([(v next) (g)])
            (loop next (sub1 fuel) (cons v seen)))))))

(test-case "an exception inside a merger invalidates the observing cursor"
  (for ([phase '(build before after)])
    (define error-value (exn:fail "injected merger failure" (current-continuation-marks)))
    (define reads 0)
    (define (broken s t)
      (when (eq? phase 'build) (raise error-value))
      (λ ()
        (when (eq? phase 'before) (raise error-value))
        (define-values (v next) (s))
        (raise error-value)))
    (define seq
      (in-amb/do
       (parameterize ([current-amb-merger broken])
         (amb (begin (set! reads (add1 reads)) 'a) 'b))))
    (define-values (more? get) (sequence-generate seq))
    (check-exn (λ (e) (eq? e error-value)) more?)
    (check-equal? reads (if (eq? phase 'after) 1 0))
    (check-exn #rx"the search cannot go on" more?)
    (check-equal? reads (if (eq? phase 'after) 1 0))))

(test-case "once, written with amb-generate*, amb-pause and amb-fail"
  ;; (for/first ([v (in-amb* thk)]) v) is the first answer too, but the
  ;; pauses of (thk) stay in its search: the search around it gets no
  ;; turn before the answer is there
  (with amb-interleave
    (check-equal? (events (amb (for/first ([v (in-amb* (λ () (amb (later 2 'a) 'b)))]) v) 'x))
                  '(b x)))
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

(test-case "a single-pass observer peeks once and preserves a zero-value answer"
  (define seen '())
  (define d
    (in-amb*/do
     (λ ()
       (call-with-values
        (λ () (make-amb (source 4 '() 3 '(2) '(3))))
        (λ vs
          (set! seen (cons vs seen))
          (apply values vs))))))
  (define-values (more? get) (sequence-generate d))
  (check-equal? seen '())
  (check-true (more?))
  (check-true (more?))
  (check-equal? seen '(()))
  (check-equal? (call-with-values get list) '())
  (check-equal? (for/first ([x d]) x) 2)
  (check-equal? (for/list ([x d]) x) '(3))
  (check-false (more?))
  (check-false (more?))
  (check-equal? (for/list ([x d]) x) '())
  (check-equal? seen '((3) (2) ())))

(test-case "a single-pass observer cannot repeat an escaped search step"
  (define leave #f)
  (define runs 0)
  (define-values (more? get)
    (sequence-generate
     (in-amb/do (amb 'first
                     (begin (amb-pause) (set! runs (add1 runs)) (leave 'left))
                     'last))))
  (check-equal? (get) 'first)
  (check-equal? (let/ec k (set! leave k) (get)) 'left)
  (check-exn #rx"the search cannot go on" more?)
  (check-exn #rx"the search cannot go on" get)
  (check-equal? runs 1))

(test-case "a single-pass observer rejects reentry without repeating the current step"
  (define runs 0)
  (define d
    (in-amb/do
     (amb 'first
          (begin
            (amb-pause)
            (set! runs (add1 runs))
            (when (= runs 1)
              (check-exn #rx"the search cannot go on" (λ () (for/first ([x d]) x))))
            'second)
          'last)))
  (check-equal? (for/list ([x d]) x) '(first second last))
  (check-equal? runs 1))

(test-case "a search that raised cannot go on"
  ;; what it has run is not run again
  (define runs 0)
  (define-values (more? get)
    (sequence-generate
     (in-amb/do (amb 1 (begin (amb-pause) (set! runs (add1 runs)) (error "boom")) 3))))
  (check-equal? (get) 1)
  (check-exn #rx"boom" get)
  (check-exn #rx"the search cannot go on" get)
  (check-exn #rx"the search cannot go on" more?)
  (check-equal? runs 1)
  ;; the stream too, with the same error; what it has read stays
  (define s (in-amb (amb 1 (begin (set! runs (add1 runs)) (error "boom")) 3)))
  (check-equal? (stream-first s) 1)
  (check-exn #rx"boom" (λ () (stream-first (stream-rest s))))
  (check-exn #rx"in-amb: the search cannot go on" (λ () (stream-first (stream-rest s))))
  (check-exn #rx"in-amb: the search cannot go on" (λ () (stream->list s)))
  (check-equal? (stream-first s) 1)
  (check-equal? runs 2)
  ;; a handler that raises is another matter: the pause is over
  (define-values (more?* get*)
    (sequence-generate
     (in-amb/do (begin (amb-pause) 'x) #:on-pause (λ () (error "handler")))))
  (check-exn #rx"handler" get*)
  (check-equal? (get*) 'x))

(test-case "an effect in a merger is not an effect of the search that it merges"
  ;; The merger pauses before it calls its generators.  The code that
  ;; interprets a choice runs outside the search that it serves, so the
  ;; pause is an effect of whatever search the merged generator is
  ;; called in: the first merge of a choice is called by the caller of
  ;; the search, after the effect left it, and the others by whoever
  ;; asks for the next event.
  (define ((pausing s t)) (amb-pause) ((amb-append s t)))
  ;; (amb 1 2) is two merges.  The observer is not in a search:
  (check-exn #rx"amb-pause: not in a search"
             (λ () (with pausing (events (amb 1 2)))))
  ;; it is in a search: that one pauses for both merges
  (check-equal? (events (with pausing (events (amb 1 2)))) '(P P (1 2)))
  ;; a supplier
  (check-exn #rx"amb-pause: not allowed while the alternatives of a choice are supplied"
             (λ () (events (sequence->amb (in-amb/do (with pausing (amb 1 2))))))))

(test-case "in-amb goes on after its handler raised, as in-amb/do does"
  ;; the example of the review of PR #24
  (define s
    (in-amb (begin (amb-pause) 42)
            #:on-pause (λ () (error 'handler "boom"))))
  (check-exn #rx"handler: boom" (λ () (stream-first s)))
  (check-equal? (stream-first s) 42)
  (check-equal? (stream->list s) '(42))
  ;; nothing is computed again, and the handler is not called again
  (define runs 0)
  (define calls 0)
  (define-syntax-rule (search)
    (begin (set! runs (add1 runs))
           (make-amb (source 3 '(a) 1 '(b)))))
  (define (handler)
    (set! calls (add1 calls))
    (if (even? calls) (error "even") calls))
  (define t (in-amb (search) #:on-pause handler))
  (define d (in-amb/do (search) #:on-pause handler))
  ;; the elements, with `raised` for a read that raised
  (define (read-all next!)
    (let loop ()
      (define x (with-handlers ([exn:fail? (λ (e) 'raised)]) (next!)))
      (if (eq? x 'end) '() (cons x (loop)))))
  (define-values (more? get) (sequence-generate d))
  (define from-sequence (read-all (λ () (if (more?) (get) 'end))))
  (check-equal? from-sequence '(1 raised 3 a raised b))
  (check-equal? (list runs calls) '(1 4))
  ;; the stream, read in the same way
  (set! calls 0)
  (define rest t)
  (define from-stream
    (read-all (λ () (if (stream-empty? rest)
                        'end
                        (begin0 (stream-first rest) (set! rest (stream-rest rest)))))))
  (check-equal? from-stream from-sequence)
  ;; and it has kept the elements that it got: the pauses whose handler
  ;; raised are not in it
  (check-equal? (stream->list t) '(1 3 a b))
  (check-equal? (stream->list t) '(1 3 a b))
  (check-equal? (list runs calls) '(2 4))
  ;; a stream that is asked for an element while it computes it
  (define u (in-amb (stream-first u)))
  (check-exn #rx"in-amb: the search cannot go on" (λ () (stream-first u))))

(test-case "the handler of a pause cannot read the element that it is producing"
  ;; the two examples of the second review of PR #24: the search ran
  ;; twice
  (define runs 0)
  (define calls 0)
  (define-syntax-rule (search) (begin (amb-pause) (set! runs (add1 runs)) 42))
  (define (reset!) (set! runs 0) (set! calls 0))
  (define reentry #rx"in-amb: an element cannot be read by the handler of its own pause")

  ;; -- the stream: every way of reading the cell that is being produced
  (for ([read (list stream-first stream-empty? stream-rest stream->list
                    (λ (s) (for/list ([x (in-stream s)]) x)))])
    (reset!)
    (define s
      (in-amb (search)
              #:on-pause (λ () (set! calls (add1 calls)) (read s))))
    ;; the handler raises, so its pause has no element
    (check-exn reentry (λ () (stream-first s)))
    (check-equal? (stream->list s) '(42))
    (check-equal? (stream->list s) '(42))
    (check-equal? (list runs calls) '(1 1)))

  ;; -- the sequence: get and more?
  (for ([use (list (λ (more? get) (get)) (λ (more? get) (more?)))])
    (reset!)
    (define-values (more? get)
      (sequence-generate
       (in-amb/do (search)
                  #:on-pause (λ () (set! calls (add1 calls)) (use more? get)))))
    (check-exn reentry get)
    (check-equal? (get) 42)
    (check-false (more?))
    (check-equal? (list runs calls) '(1 1)))

  ;; -- a handler that catches the error and returns: its values are
  ;; the element of the pause, and the search goes on once
  (reset!)
  (define s
    (in-amb (search)
            #:on-pause (λ ()
                         (set! calls (add1 calls))
                         (with-handlers ([exn:fail? (λ (e) 'caught)])
                           (stream-first s)))))
  (check-equal? (stream->list s) '(caught 42))
  (check-equal? (stream->list s) '(caught 42))
  (check-equal? (list runs calls) '(1 1))
  (reset!)
  (define-values (more? get)
    (sequence-generate
     (in-amb/do (search)
                #:on-pause (λ ()
                             (set! calls (add1 calls))
                             (with-handlers ([exn:fail? (λ (e) 'caught)])
                               (get))))))
  (check-equal? (list (get) (get) (more?)) '(caught 42 #f))
  (check-equal? (list runs calls) '(1 1))

  ;; -- a handler that catches the error and raises another one
  (reset!)
  (define t
    (in-amb (search)
            #:on-pause (λ ()
                         (set! calls (add1 calls))
                         (with-handlers ([exn:fail? (λ (e) (error "another"))])
                           (stream-first t)))))
  (check-exn #rx"another" (λ () (stream-first t)))
  (check-equal? (stream->list t) '(42))
  (check-equal? (list runs calls) '(1 1))

  ;; -- the handler may read what the stream has read before
  (reset!)
  (define u
    (in-amb (begin (amb-pause) (amb-pause) (set! runs (add1 runs)) 42)
            #:on-pause (λ ()
                         (set! calls (add1 calls))
                         (if (= calls 1) 'first (list 'after (stream-first u))))))
  (check-equal? (stream->list u) '(first (after first) 42))
  (check-equal? (list runs calls) '(1 2))

  ;; -- and another search, with a handler of its own that reads this one
  (reset!)
  (define v
    (in-amb (search)
            #:on-pause (λ ()
                         (set! calls (add1 calls))
                         (stream->list
                          (in-amb (begin (amb-pause) 'inner)
                                  #:on-pause (λ ()
                                               (with-handlers ([exn:fail? (λ (e) 'caught)])
                                                 (stream-first v))))))))
  (check-equal? (stream->list v) '((caught inner) 42))
  (check-equal? (list runs calls) '(1 1)))

(test-case "a prompt between the handler and the read does not hide the handler"
  ;; the examples of the third review of PR #24
  (define runs 0)
  (define calls 0)
  (define-syntax-rule (search) (begin (amb-pause) (set! runs (add1 runs)) 42))
  (define (reset!) (set! runs 0) (set! calls 0))
  (define reentry #rx"in-amb: an element cannot be read by the handler of its own pause")
  (define tag (make-continuation-prompt-tag 'other))
  (define wraps
    (list call-with-continuation-prompt
          (λ (thk) (call-with-continuation-prompt thk tag))
          (λ (thk) (call-with-continuation-prompt
                    (λ () (call-with-continuation-prompt thk tag))))
          ;; a search is a prompt, too
          (λ (thk) (stream-first (in-amb* thk)))))

  (for* ([wrap wraps]
         [read (list stream-first stream-empty? stream-rest)])
    ;; -- not caught: the pause has no element; read again
    (reset!)
    (define s
      (in-amb (search)
              #:on-pause (λ () (set! calls (add1 calls)) (wrap (λ () (read s))))))
    (check-exn reentry (λ () (stream-first s)))
    (check-equal? (stream->list s) '(42))
    (check-equal? (stream->list s) '(42))
    (check-equal? (list runs calls) '(1 1))
    ;; -- caught, inside the prompt and outside of it
    (for ([catch (list (λ (thk) (wrap (λ () (with-handlers ([exn:fail? (λ (e) 'caught)]) (thk)))))
                       (λ (thk) (with-handlers ([exn:fail? (λ (e) 'caught)]) (wrap thk))))])
      (reset!)
      (define t
        (in-amb (search)
                #:on-pause (λ () (set! calls (add1 calls)) (catch (λ () (read t))))))
      (check-equal? (stream->list t) '(caught 42))
      (check-equal? (stream->list t) '(caught 42))
      (check-equal? (list runs calls) '(1 1))))

  (for* ([wrap wraps]
         [use (list (λ (more? get) (get)) (λ (more? get) (more?)))])
    (reset!)
    (define-values (more? get)
      (sequence-generate
       (in-amb/do (search)
                  #:on-pause (λ () (set! calls (add1 calls))
                               (wrap (λ () (use more? get)))))))
    (check-exn reentry get)
    (check-equal? (get) 42)
    (check-false (more?))
    (check-equal? (list runs calls) '(1 1))
    (reset!)
    (define-values (more?* get*)
      (sequence-generate
       (in-amb/do (search)
                  #:on-pause (λ () (set! calls (add1 calls))
                               (with-handlers ([exn:fail? (λ (e) 'caught)])
                                 (wrap (λ () (use more?* get*))))))))
    (check-equal? (list (get*) (get*) (more?*)) '(caught 42 #f))
    (check-equal? (list runs calls) '(1 1)))

  ;; -- observers inside observers, with prompts between them: the
  ;; innermost handler reads the element of each of the outer ones
  (for ([wrap wraps])
    (reset!)
    (define inner-runs 0)
    (define inner-calls 0)
    (define seen '())
    (define (try thk)
      (with-handlers ([exn:fail? (λ (e) (regexp-match? reentry (exn-message e)))])
        (wrap thk)))
    (define outer
      (in-amb (search)
              #:on-pause
              (λ ()
                (set! calls (add1 calls))
                (define middle
                  (in-amb (begin (amb-pause) (set! inner-runs (add1 inner-runs)) 'middle)
                          #:on-pause
                          (λ ()
                            (set! inner-calls (add1 inner-calls))
                            (set! seen (list (try (λ () (stream-first outer)))
                                             (try (λ () (stream-first middle)))))
                            'paused)))
                (wrap (λ () (stream->list middle))))))
    (check-equal? (stream->list outer) '((paused middle) 42))
    (check-equal? (stream->list outer) '((paused middle) 42))
    (check-equal? seen '(#t #t))
    (check-equal? (list runs calls inner-runs inner-calls) '(1 1 1 1))))

(test-case "a handler that was left is not taken for one that is running"
  ;; every way out of the handler: the pause is over, and the search
  ;; goes on once
  (define tag (make-continuation-prompt-tag 'other))
  (for ([leave (list (λ () (error "boom"))
                     (λ () (raise 'not-an-exn))
                     (λ () (abort-current-continuation (default-continuation-prompt-tag)
                                                       (λ () 'aborted)))
                     (λ () (abort-current-continuation tag 'aborted))
                     (λ () (call-with-continuation-prompt (λ () (error "boom")))))])
    (define runs 0)
    (define calls 0)
    (define (read s)
      (let/ec k
        (call-with-continuation-prompt
         (λ ()
           (call-with-continuation-prompt
            (λ () (with-handlers ([(λ (e) #t) (λ (e) (k 'left))])
                    (stream-first s)))
            (default-continuation-prompt-tag)
            (λ (thk) (k 'left))))
         tag
         (λ (v) (k 'left)))))
    (define s
      (in-amb (begin (amb-pause) (amb-pause) (set! runs (add1 runs)) 42)
              #:on-pause (λ () (set! calls (add1 calls)) (leave))))
    ;; two pauses, both left; then the answer
    (check-equal? (read s) 'left)
    (check-equal? (read s) 'left)
    (check-equal? (read s) 42)
    (check-equal? (stream->list s) '(42))
    (check-equal? (list runs calls) '(1 2)))

  ;; a jump out through an escape continuation, with the sequence
  (define runs 0)
  (define calls 0)
  (define out #f)
  (define-values (more? get)
    (sequence-generate
     (in-amb/do (begin (amb-pause) (set! runs (add1 runs)) 42)
                #:on-pause (λ () (set! calls (add1 calls)) (out 'left)))))
  (check-equal? (let/ec k (set! out k) (get)) 'left)
  (check-equal? (get) 42)
  (check-false (more?))
  (check-equal? (list runs calls) '(1 1)))
