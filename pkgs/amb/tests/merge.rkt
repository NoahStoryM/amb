#lang racket/base

;; amb/merge against the equations of spec-merge.rkt, in the two senses
;; of V3-PLAN section 3:
;;
;;   the same events   n pauses being n times one pause; up to the end,
;;                     or up to the call that does not return (here: the
;;                     call that raises);
;;   the same calls    before every event, the same generators have been
;;                     called, in the same order.
;;
;; The raw results are not compared: they are not the same.
;;
;; Both sides are given the same leaves, in the same pieces; what is
;; ignored is how often the mergers call each other.  The whole log is
;; compared too, so the calls after the last event count: those before
;; the end, and those before a call that raises.  This is a test of
;; finite runs: a call that does not return is only stood for by one
;; that raises.

(require rackunit
         racket/list
         racket/match
         (prefix-in spec: "spec-merge.rkt")
         (prefix-in flat: "../merge.rkt")
         (only-in "../private/generator.rkt" end)
         "../private/data/catenable.rkt")

(define (spec-merge m)
  (case m
    [(append) spec:amb-append]
    [(interleave) spec:amb-interleave]
    [(alternate) spec:amb-alternate]
    [(level) spec:amb-level]))
(define (flat-merge m)
  (case m
    [(append) flat:amb-append]
    [(interleave) flat:amb-interleave]
    [(alternate) flat:amb-alternate]
    [(level) flat:amb-level]))
(define strategies '(append interleave alternate level))

(define (pauses n g) (if (zero? n) g (λ () (values 1 (pauses (sub1 n) g)))))


;; ---- generators that keep a log ----

;; The log of a run: the calls of the leaves, the last one first, and
;; how many they are.
(define log (make-parameter #f))
(define calls (make-parameter #f))
(define checkpoint (make-parameter void))
(define call-limit (make-parameter +inf.0))
(define (note! x)
  (set-box! (log) (cons x (unbox (log))))
  (set-box! (calls) (add1 (unbox (calls))))
  ((checkpoint) (list 'call x))
  (when (>= (unbox (calls)) (call-limit))
    (error 'source "call limit reached")))
(define-syntax-rule (with-log e ...)
  (parameterize ([log (box '())] [calls (box 0)]) e ...))

;; A leaf is a list of raw results:
;;   (list v)   an answer
;;   n          n pauses
;;   boom       the call raises: it stands for a call that does not
;;              return
;;   0          not a result: the merger that sees it raises
;; and then it ends.  Nothing may be called after the end.
(define (leaf id raws)
  (let loop ([raws raws] [i 0])
    (define used? #f)
    (λ ()
      ;; Test failures must escape the handler for injected exceptions.
      (when used? (raise (list 'single-pass-violation id i)))
      (set! used? #t)
      (note! (cons id i))
      (match raws
        ['() (values #f (λ () (raise (list 'called-after-end id))))]
        [(cons 'boom _) (error "boom" id)]
        [(cons r raws) (values r (loop raws (add1 i)))]))))


;; ---- expressions ----
;;
;;   e ::= (leaf id raws)
;;       | (m e e)            a merge, m one of the four strategies
;;       | (drop k e)         e, after k of its events were taken
;;       | (fault id phase m e e)
;;                           a merger that raises while being built,
;;                           before reading, or after reading an operand
;;       | (silent id)       infinitely many source calls, no output;
;;                           the test's call budget interrupts it

(define (silent id)
  (define ((source i))
    (note! (cons id i))
    (values '(ignored) (source (add1 i))))
  (λ ()
    (let loop ([g (source 0)])
      (define-values (v next) (g))
      (loop next))))

;; A merger outside amb/merge is opaque to fusion.  Throw from its own
;; code, not from a leaf, including after its operand has advanced.
(define ((fault id phase merge) s t)
  (define (boom) (error 'merger "injected ~s ~s" id phase))
  (when (eq? phase 'build) (boom))
  (define gen (merge s t))
  (λ ()
    (when (eq? phase 'before) (boom))
    (define-values (v next) (gen))
    (boom)))

(define (build merge-of e)
  (let build ([e e])
    (match e
      [`(leaf ,id ,raws) (leaf id raws)]
      [`(silent ,id) (silent id)]
      [`(drop ,k ,e) (drop k (build e))]
      [`(fault ,id ,phase ,m ,l ,r)
       ((fault id phase (merge-of m)) (build l) (build r))]
      [`(,m ,l ,r) ((merge-of m) (build l) (build r))])))

;; What is left of g after k events.
(define (drop k g)
  (if (zero? k)
      g
      (let-values ([(v next) (g)])
        (cond
          [(not v) (λ () (values #f (λ () (error "called after the end"))))]
          [(list? v) (drop (sub1 k) next)]
          [(eqv? v 0) (error "zero")]
          [(<= v k) (drop (- k v) next)]
          [else (pauses (- v k) next)]))))

;; The events of g, each with the number of calls logged when it came:
;; an answer, pause, end; or (raised message) for the call that raised.
(define (observe g fuel)
  (define (stamp e)
    ((checkpoint) (list 'event e))
    (cons e (unbox (calls))))
  (let loop ([g g] [fuel fuel])
    (if (zero? fuel)
        '()
        (let-values ([(v next)
                      (with-handlers ([exn:fail? (λ (e) (values 'raised (exn-message e)))])
                        (g))])
          (cond
            [(eq? v 'raised) (list (stamp (list 'raised next)))]
            [(not v) (list (stamp 'end))]
            [(list? v) (cons (stamp v) (loop next (sub1 fuel)))]
            [(eqv? v 0) (list (stamp 'zero))]
            [else (cons (stamp 'pause) (loop (pauses (sub1 v) next) (sub1 fuel)))])))))

;; The events with their stamps, and the log, of the expression e.
(define (run merge-of e fuel)
  (with-log
    (define events
      (with-handlers ([exn:fail? (λ (x) (list 'raised-while-building (exn-message x)))])
        (observe (build merge-of e) fuel)))
    (list events (reverse (unbox (log))))))

(define (events-of result) (map car (car result)))


;; ---- random expressions ----

(define next-id 0)
(define (fresh-id) (set! next-id (add1 next-id)) next-id)

(define (random-raws bombs?)
  (define raws
    (for/list ([_ (random 5)])
      (case (random 8)
        [(0 1 2) (list (random 100))]
        [(3 4 5) 1]
        [else (add1 (random 4))])))
  (if (and bombs? (zero? (random 8)))
      (append raws (list (if (zero? (random 3)) 0 'boom)))
      raws))

(define (random-expression size strategies bombs? [left 3])
  (let gen ([size size])
    (cond
      [(<= size 1) `(leaf ,(fresh-id) ,(random-raws bombs?))]
      [(zero? (random 6)) `(drop ,(random 4) ,(gen size))]
      [else
       ;; mostly nested on the left
       (define l (if (zero? (random left))
                     (add1 (random (sub1 size)))
                     (max 1 (- size 1 (random 2)))))
       (define l* (min l (sub1 size)))
       `(,(list-ref strategies (random (length strategies)))
         ,(gen l*) ,(gen (- size l*)))])))

(define (check-same e fuel)
  (define expected (run spec-merge e fuel))
  (define actual (run flat-merge e fuel))
  (unless (equal? actual expected)
    (fail-check (format "~s\n  fuel ~a\n  equations ~s\n  flat      ~s" e fuel expected actual))))


;; ---- the tests ----

(test-case "append hands off to its only remaining generator without calling it"
  (for ([event (list '() '(left) 3)])
    (define calls 0)
    (define (right)
      (set! calls (add1 calls))
      (values '(right) end))
    (define-values (v next)
      ((flat:amb-append (λ () (values event end)) right)))
    (check-equal? v event)
    (check-equal? calls 0)
    (check-eq? next right)
    (check-equal? (call-with-values next list) (list '(right) end))
    (check-equal? calls 1)))

(test-case "append keeps a chaperoned generator opaque to an enclosing merger"
  (define ((answer v)) (values (list v) end))
  (define guarded
    (chaperone-procedure (flat:amb-level (answer 2) (answer 3))
                         (λ () (error 'guarded "denied"))))
  (define-values (v next)
    ((flat:amb-level (flat:amb-append (answer 1) guarded) (answer 4))))
  (check-equal? v '(1))
  (check-exn #rx"^guarded: denied" next))

(test-case "examples"
  (define (events m . leaves)
    (events-of
     (run flat-merge
          (for/fold ([e #f]) ([raws leaves] [i (in-naturals)])
            (define l `(leaf ,i ,raws))
            (if e `(,m ,e ,l) l))
          100)))
  (check-equal? (events 'append '((a) 2 (b)) '() '((c) 1))
                '((a) pause pause (b) (c) pause end))
  ;; rounds: 3 pauses are three rounds
  (check-equal? (events 'level '((a) 3 (b)) '((c) 1 (d)) '(1 1 (e) 1))
                '((a) (c) pause (d) pause (e) pause (b) end))
  ;; the last one goes on by itself
  (check-equal? (events 'level '() '(2 (a)) '()) '(pause pause (a) end)))

(test-case "flat = equations: events and calls, only the two flat strategies"
  (for ([m '(append level)])
    (for* ([size '(2 3 5 8 12)] [_ 400])
      (check-same (random-expression size (list m) #f) (random 30)))))

(test-case "flat = equations: the four strategies mixed"
  (for* ([size '(2 3 5 8 12 20)] [_ 500])
    (check-same (random-expression size strategies #f) (random 40))))

(test-case "flat = equations: generators that raise, or return 0"
  (for* ([size '(2 3 5 8 12)] [_ 500])
    (check-same (random-expression size strategies #t) (random 40)))
  (for* ([m '(append level)] [size '(3 6 10)] [_ 300])
    (check-same (random-expression size (list m) #t) (random 40))))

(test-case "merger code can raise before or after reading an operand"
  (for* ([outer strategies] [inner strategies] [phase '(build before after)])
    (define broken `(fault boom ,phase ,inner (leaf a ((a) 3)) (leaf b (2 (b)))))
    ;; On either side, and with other mergers of the same kind around it.
    (define expressions
      (list `(,outer (,outer ,broken (leaf c ((c)))) (leaf d ((d))))
            `(,outer (leaf c (1 (c))) (,outer ,broken (leaf d ((d)))))))
    (for ([e expressions])
      (check-same e 30))))

(test-case "compare source-call prefixes while each call is running"
  (define (online e fuel)
    (define tape '())
    (define expected
      (parameterize ([checkpoint (λ (x) (set! tape (cons x tape)))])
        (run spec-merge e fuel)))
    (define remaining (reverse tape))
    ;; A mismatch escapes the observer's exn:fail? handler immediately.
    (define actual
      (parameterize ([checkpoint
                      (λ (x)
                        (unless (and (pair? remaining) (equal? x (car remaining)))
                          (raise (list 'prefix-mismatch e x remaining)))
                        (set! remaining (cdr remaining)))])
        (run flat-merge e fuel)))
    (check-equal? remaining '())
    (check-equal? actual expected))
  (parameterize ([current-pseudo-random-generator (make-pseudo-random-generator)])
    (random-seed 20261115)
    (for* ([size '(2 5 12 20)] [_ 100])
      (online (random-expression size strategies #t) (random 40))))
  (for* ([m strategies] [phase '(build before after)])
    (online `(,m (leaf a (1 (a)))
                 (fault boom ,phase ,m (leaf b ((b))) (leaf c (2 (c)))))
            30))
  ;; No output event is reached.  Check calls as they happen, then
  ;; interrupt at a known source-call boundary.  This is not a proof
  ;; of divergence, nor a timeout used as a semantic expectation.
  (parameterize ([call-limit 17])
    (for ([m strategies])
      (online `(,m (leaf a ((a))) (,m (silent b) (leaf c ((c))))) 30))))

(test-case "nothing is called after the last event that is asked for"
  ;; every leaf ends in a call that raises; with fuel for all the events
  ;; before it, the call is not made
  (for* ([m '(append level)] [_ 300])
    (define e (random-expression 6 (list m) #f))
    (define all (events-of (run spec-merge e 1000)))
    (for ([fuel (in-range (length all))])
      (check-same e fuel))))

(test-case "the events do not depend on how the pauses are written"
  ;; the same events, written in another way: the counts of pauses cut
  ;; up or put together
  (define (rewrite raws)
    (define ones
      (append* (for/list ([r raws])
                 (cond [(list? r) (list r)]
                       [else (make-list r 1)]))))
    (let loop ([ones ones])
      (cond
        [(null? ones) '()]
        [(list? (car ones)) (cons (car ones) (loop (cdr ones)))]
        [else
         (define-values (ps rest) (splitf-at ones (λ (x) (eqv? x 1))))
         (define n (add1 (random (length ps))))
         (cons n (loop (append (make-list (- (length ps) n) 1) rest)))])))
  (define (rewrite-leaves e)
    (match e
      [`(leaf ,id ,raws) `(leaf ,id ,(rewrite raws))]
      [`(drop ,k ,e) `(drop ,k ,(rewrite-leaves e))]
      [`(,m ,l ,r) `(,m ,(rewrite-leaves l) ,(rewrite-leaves r))]))
  (for* ([size '(2 3 5 8 12)] [_ 400])
    (define e (random-expression size strategies #f))
    (check-equal? (events-of (run flat-merge (rewrite-leaves e) 1000))
                  (events-of (run flat-merge e 1000)))))

(test-case "the steps of the lists"
  ;; a merge nested n deep on the left; and a stream that is merged again
  ;; after each of its events
  (define (left m n)
    (for/fold ([g (leaf 0 '((0)))]) ([i (in-range 1 n)])
      ((flat-merge m) g (leaf i (list (list i))))))
  (define (again m n)
    (for/fold ([g (leaf 0 (for/list ([i n]) (list i)))]) ([i n])
      (define-values (v next) (((flat-merge m) g (leaf (- -1 i) '()))))
      next))
  (for* ([m '(append level)] [n '(1000 10000 100000)])
    (with-log
      (define before (cat-steps))
      (check-equal? (for/sum ([e (observe (left m n) (* 2 n))]) (if (pair? (car e)) 1 0)) n)
      (check-true (<= (- (cat-steps) before) (* 4 n))))
    (with-log
      (define before (cat-steps))
      (check-equal? (map car (observe (again m n) 10)) '(end))
      (check-true (<= (- (cat-steps) before) (* 4 n))))))
