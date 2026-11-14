#lang racket/base

(require rackunit racket/list racket/match racket/sequence racket/port
         "kernel.rkt" (prefix-in old: "../choice-as-merge/model.rkt"))

(define merges (list amb-append amb-interleave amb-alternate amb-level))
(define names '(append interleave alternate level))
(define comparisons 0)
(define (same actual expected)
  (set! comparisons (add1 comparisons))
  (check-equal? actual expected))
(define (prefix seq fuel)
  (for/list ([_ (in-range fuel)] [x seq]) x))
(define (observe thk merge fuel)
  (parameterize ([current-amb-merge merge])
    (prefix (in-amb (list 'answer (call-with-values thk list))
                   #:on-pause (λ () 'pause)) fuel)))

;; Tests alone use event words and an adapter for the old oracle's constructors.
;; No adapters or event structures are part of the new reference kernel.
(define (source es)
  (λ ()
    (if (null? es) (values #f (λ () (error "end tail forced")))
        (values (car es) (source (cdr es))))))
(define (old-source es)
  (cond
    [(null? es) '()]
    [(list? (car es)) (cons (car es) (λ () (old-source (cdr es))))]
    [else
     (let repeat ([n (car es)])
       (if (= n 1) (λ () (old-source (cdr es)))
           (λ () (repeat (sub1 n)))))]))
(define (collect g fuel)
  (let loop ([g g] [fuel fuel])
    (cond
      [(zero? fuel) '()]
      [else
       (define-values (r next) (g))
       (cond
         [(not r) '(end)]
         [(list? r) (cons (list 'answer r) (loop next (sub1 fuel)))]
         [else
          (define n (min fuel r)) ; both are exact integers
          (append (make-list n 'pause)
                  (if (= n fuel) '() (loop next (- fuel n))))])])))
(define (old-collect s fuel)
  (cond
    [(zero? fuel) '()]
    [(null? s) '(end)]
    [(pair? s)
     (cons (list 'answer (car s))
           (if (= fuel 1) '() (old-collect ((cdr s)) (sub1 fuel))))]
    [else (cons 'pause (if (= fuel 1) '() (old-collect (s) (sub1 fuel))))]))

(test-case "binary choice, nesting, failure, and the entire continuation"
  (same (observe (λ () (+ 10 (amb 1 2))) amb-append 10)
        '((answer (11)) (answer (12))))
  (same (observe (λ () (list (amb 1 2) (amb 'a 'b))) amb-append 10)
        '((answer ((1 a))) (answer ((1 b))) (answer ((2 a))) (answer ((2 b)))))
  (same (observe (λ () (amb (amb) 4 (begin (amb-fail) 'unreachable))) amb-append 10)
        '((answer (4))))
  (same (observe (λ () (amb)) amb-append 10) '())
  (same (observe (λ () (amb-fail)) amb-append 10) '())
  (same (observe (λ () (amb 9)) amb-append 10) '((answer (9)))))

(test-case "zero values and multiple values stay one sequence element"
  (for ([merge merges])
    (same (observe (λ () (amb (values) (values 1 2) (values #f 'pause))) merge 10)
          '((answer ()) (answer (1 2)) (answer (#f pause)))))
  (same (for/list ([(x y) (in-amb (amb (values 1 2) (values 3 4)))]) (+ x y)) '(3 7))
  (same (for/list ([vs (sequence-map list (in-amb (values)))]) vs) '(())))

(test-case "pause is distinct from failure; infinite computations have fuel"
  (define (never) (amb-pause) (never))
  (for ([merge merges])
    (same (observe (λ () (amb-pause) 'x) merge 10) '(pause (answer (x))))
    (same (observe never merge 8) (make-list 8 'pause)))
  (for ([merge (cdr merges)])
    (define trace (observe (λ () (amb (never) 'reachable)) merge 8))
    (check-not-false (member '(answer (reachable)) trace)))
  (same (observe (λ () (amb (never) 'reachable)) amb-append 8) (make-list 8 'pause)))

(test-case "merge equations, counts and every bounded event prefix"
  (random-seed 20261114)
  (for* ([merge merges] [n (in-range 1 7)] [m (in-range 1 7)] [fuel '(0 1 2 3 8 30)])
    (define which (list-ref names (index-of merges merge)))
    (define left (list n '(left) 1 '(last-left)))
    (define right (list m '(right) 2 '(last-right)))
    (same (collect (merge (source left) (source right)) fuel)
          (old-collect ((old:reference-merge which) (old-source left)
                         (λ () (old-source right))) fuel)))
  (define (random-events)
    (for/list ([_ (in-range (random 10))])
      (case (random 4) [(0) '()] [(1) (list (random 20) #f)]
            [(2) (list (random 20))] [else (add1 (random 5))])))
  (for* ([merge merges] [_ (in-range 300)])
    (define which (list-ref names (index-of merges merge)))
    (define l (random-events))
    (define r (random-events))
    (define fuel (random 40))
    (same (collect (merge (source l) (source r)) fuel)
          (old-collect ((old:reference-merge which) (old-source l) (λ () (old-source r))) fuel)))
  (same (collect (amb-level (source '(5 (a))) (source '(3 (b)))) 20)
        '(pause pause pause (answer (b)) pause pause (answer (a)) end)))

(test-case "mixed nested merge expressions agree with the unchanged oracle"
  (define (gen n)
    (if (= n 1)
        `(leaf ,(for/list ([_ (in-range (random 6))])
                  (if (zero? (random 2)) (add1 (random 4)) (list (random 8)))))
        (let ([l (add1 (random (sub1 n)))])
          `(,(list-ref names (random 4)) ,(gen l) ,(gen (- n l))))))
  (define (run e new?)
    (match e
      [`(leaf ,es) (if new? (source es) (old-source es))]
      [`(,m ,s ,t)
       (define i (index-of names m))
       (if new? ((list-ref merges i) (run s #t) (run t #t))
           ((old:reference-merge m) (run s #f) (λ () (run t #f))))]))
  (for ([_ (in-range 800)])
    (define e (gen (add1 (random 10))))
    (define fuel (random 60))
    (same (collect (run e #t) fuel) (old-collect (run e #f) fuel))))

;; Independent direct-CPS model: no new kernel code is used by old:spec.
;; For alt m, parameterize only make-amb; invoking its chosen thunk outside
;; that parameterize restores the original ambient strategy inside the branch.
(define (run-goal g state)
  (match g
    [`(tag ,x) (cons x state)]
    ['succeed state]
    ['fail (amb-fail)]
    ['pause (amb-pause) state]
    [`(seq ,a ,b) (run-goal b (run-goal a state))]
    [`(alt ,a ,b) (amb (run-goal a state) (run-goal b state))]
    [`(alt ,m ,a ,b)
     ((parameterize ([current-amb-merge (list-ref merges (index-of names m))])
        (make-amb (λ () (sequence-generate*
                        (list (λ () (run-goal a state)) (λ () (run-goal b state))))))))]))

(test-case "choice as merge: random direct programs equal the explicit-CPS spec"
  (for* ([name names] [_ (in-range 500)])
    (define merge (list-ref merges (index-of names name)))
    (define g (old:random-goal (add1 (random 14)) '(seq alt alt*)))
    (define expected
      (map (λ (e) (if (eq? e 'pause) 'pause (list 'answer (list e)))) ((old:spec name) g)))
    (for ([fuel '(0 1 3 12 120)])
      (same (observe (λ () (run-goal g '(start))) merge fuel)
            (take expected (min fuel (length expected)))))))

(test-case "creating a sequence, initializing it and asking more twice stay lazy"
  (define n 0)
  (define seq (in-amb (begin (set! n (add1 n)) (amb 1 2))))
  (same n 0)
  (define-values (more? get) (sequence-generate seq))
  (same n 0)
  (check-true (more?))
  (check-true (more?))
  (same n 1)
  (same (get) 1)
  (check-true (more?))
  (same (get) 2)
  (check-false (more?))
  (check-false (more?))
  (check-exn #rx"single-pass" (λ () (sequence->list seq)))
  (same (prefix (in-amb (error "zero fuel executed")) 0) '()))

(test-case "unselected branches and tails never run before their turn"
  (define log '())
  (define (record x) (set! log (append log (list x))) x)
  (define seq (in-amb (+ 10 (amb (record 1) (record 2)))))
  (same log '())
  (same (prefix seq 1) '(11))
  (same log '(1))
  (same (prefix (in-amb (amb 1 (error "early branch"))) 1) '(1))
  (same (prefix (in-amb (amb-pause) #:on-pause (λ () 'pause)) 1) '(pause))
  (same (prefix (in-amb (begin (amb-pause) (error "early resume"))
                        #:on-pause (λ () 'pause)) 1) '(pause))
  (for ([merge merges])
    (same (collect (merge (source '((first))) (λ () (error "early right"))) 1)
          '((answer (first)))))
  (define n 0)
  (define counted (λ () (values 1000000000 (λ () (set! n 1) (values '(x) void)))))
  (same (observe (λ () (make-amb counted)) amb-append 4) (make-list 4 'pause))
  (same n 0))

(test-case "choice strategy is read at execution and fixed for that choice"
  (define seq
    (parameterize ([current-amb-merge amb-append])
      (in-amb (amb (begin (amb-pause) 'a) 'b) #:on-pause (λ () 'pause))))
  (define-values (more? get) (sequence-generate seq))
  (parameterize ([current-amb-merge amb-interleave])
    (check-true (more?))
    (same (get) 'pause))
  ;; Switching the ambient strategy does not change the existing choice.
  (parameterize ([current-amb-merge amb-append])
    (same (list (get) (get)) '(b a)))
  (define later
    (in-amb (begin (amb-pause) (amb (begin (amb-pause) 'a) 'b))
            #:on-pause (λ () 'pause)))
  (define-values (later? next) (sequence-generate later))
  (parameterize ([current-amb-merge amb-append]) (same (next) 'pause))
  (parameterize ([current-amb-merge amb-interleave])
    (same (list (next) (next) (next)) '(pause b a)))
  (parameterize ([current-amb-merge amb-append])
    (same (prefix (in-amb (parameterize ([current-amb-merge amb-interleave])
                           (amb (begin (amb-pause) 'a) 'b))
                         #:on-pause (λ () 'pause)) 5) '(pause b a))))

(test-case "phase marks do not freeze unrelated ambient parameters"
  (define p (make-parameter 'default))
  (define seq (in-amb (begin (amb-pause) (p)) #:on-pause (λ () 'pause)))
  (define-values (more? get) (sequence-generate seq))
  (parameterize ([p 'first]) (same (get) 'pause))
  (parameterize ([p 'second]) (same (get) 'second))
  (define fixed
    (in-amb (parameterize ([p 'inside]) (amb-pause) (p)) #:on-pause (λ () 'pause)))
  (define-values (fixed? next) (sequence-generate fixed))
  (parameterize ([p 'outside]) (same (list (next) (next)) '(pause inside))))

(test-case "each delivered event has the required side-effect prefix"
  (for ([merge merges])
    (define log '())
    (define (record x) (set! log (append log (list x))))
    (define (branch start resume value)
      (record start) (amb-pause) (record resume) value)
    (define actual
      (parameterize ([current-amb-merge merge])
        (for/list ([_ (in-range 6)]
                   [e (in-amb (amb (branch 'l-start 'l-resume 'L)
                                   (branch 'r-start 'r-resume 'R))
                              #:on-pause (λ () 'pause))])
          (list e log))))
    (same actual
          (cond
            [(eq? merge amb-append)
             '((pause (l-start)) (L (l-start l-resume))
               (pause (l-start l-resume r-start)) (R (l-start l-resume r-start r-resume)))]
            [(eq? merge amb-level)
             '((pause (l-start r-start)) (L (l-start r-start l-resume))
               (R (l-start r-start l-resume r-resume)))]
            [else
             '((pause (l-start)) (pause (l-start r-start))
               (L (l-start r-start l-resume)) (R (l-start r-start l-resume r-resume)))]))))

(test-case "counted supplier pauses agree with explicit amb-pause in every merge"
  (for* ([merge merges] [n (in-range 1 9)] [fuel '(0 1 2 3 10 25)])
    (same (observe (λ () (amb (make-amb (source (list n '(a)))) 'b)) merge fuel)
          (observe (λ () (amb (begin (for ([_ (in-range n)]) (amb-pause)) 'a) 'b))
                   merge fuel))))

(test-case "a paused continuation preserves its dynamic-wind boundaries"
  (define log '())
  (define (record x) (set! log (append log (list x))))
  (define seq
    (in-amb (dynamic-wind (λ () (record 'enter))
                         (λ () (amb-pause) (record 'body) 42)
                         (λ () (record 'exit)))
            #:on-pause (λ () 'pause)))
  (same log '())
  (define-values (more? get) (sequence-generate seq))
  (same (get) 'pause)
  (same log '(enter exit))
  (same (get) 42)
  (same log '(enter exit enter body exit)))

(test-case "native sequence generators, zero/multiple values and single-pass ports"
  (for ([seq (list (in-range 10) (in-list '(a b c)) (in-naturals))])
    (same (prefix (in-amb (make-amb (λ () (sequence-generate* seq)))) 5)
          (for/list ([_ (in-range 5)] [x seq]) x)))
  (same (for/list ([(x y) (in-amb (make-amb
                                  (λ () (sequence-generate*
                                         (in-parallel (in-range 3) '(a b c))))))])
          (list x y)) '((0 a) (1 b) (2 c)))
  (define zero-seq
    (make-do-sequence (λ () (values (λ (_) (values)) add1 0 (λ (i) (< i 3)) #f #f))))
  (same (for/list ([vs (sequence-map list (in-amb (make-amb
                                                 (λ () (sequence-generate* zero-seq)))))]) vs)
        '(() () ()))
  (define port (open-input-string "abc"))
  (define seq (in-amb (make-amb (λ () (sequence-generate* (in-port read-char port))))))
  (same (peek-char port) #\a)
  (same (prefix seq 1) '(#\a))
  (same (peek-char port) #\b)
  (same (observe (λ () (make-amb (λ () (sequence-generate* '())))) amb-append 5) '())
  (for ([invalid '(0 -1 #t 1.5)])
    (check-exn exn:fail:contract?
               (λ () (prefix (in-amb (make-amb (λ () (values invalid void)))) 1)))))

(test-case "for/amb derives lazy branches, not eager or frozen body results"
  (same (prefix (in-amb (for/amb ([i (in-range 4)]) (* i i))) 10) '(0 1 4 9))
  (same (prefix (in-amb (for/amb ([i '(1 2)]) (amb i (- i)))) 10) '(1 -1 2 -2))
  (same (prefix (in-amb (for/amb ([i '(1 2)]) (amb-fail))) 10) '())
  (same (prefix (in-amb (for/amb ([i (in-naturals)]) i)) 5) '(0 1 2 3 4))
  (same (prefix (in-amb (for/amb ([i '(0 1 2)] #:when (positive? i)) #:break (= i 2) i)) 10)
        '(1))
  (same (prefix (in-amb (for/amb ([i '(0 1 2)] #:final (= i 1)) i)) 10) '(0 1))
  (same (for/list ([(x y) (in-amb (for/amb ([i 3]) (values i (- i))))]) (list x y))
        '((0 0) (1 -1) (2 -2))))

(test-case "supplier effects are diagnosed, including delayed and indirect clauses"
  (define (bad) (amb #t #f))
  (define programs
    (list (λ () (for/amb ([x (in-range (amb 1 2))]) x))
          (λ () (for/amb ([x '(1 2)] #:when (amb #t #f)) x))
          (λ () (for/amb ([x '(1 2)]) #:break (bad) x))
          (λ () (for/amb ([x '(1 2)] #:when (begin (amb-fail) #t)) x))
          (λ () (for/amb ([x '(1 2)] #:when (begin (amb-pause) #t)) x))
          (λ () (for/amb ([x '(1 2)]) #:final (bad) x))
          (λ () (make-amb (λ () (amb-pause) (values '(x) void))))))
  (for ([program programs])
    (check-exn #rx"supplying alternatives" (λ () (observe program amb-append 5))))
  (define (later-bad)
    (for/amb ([x '(1 2)] #:when (if (= x 1) #t (bad))) x))
  (same (observe later-bad amb-append 1) '((answer (1))))
  (check-exn #rx"supplying alternatives" (λ () (observe later-bad amb-append 2)))
  ;; Source protocol hooks run in the supply phase as well.
  (for ([stage '(init value next end)])
    (define seq
      (make-do-sequence
       (λ ()
         (when (eq? stage 'init) (amb-pause))
         (values (λ (i) (when (eq? stage 'value) (amb-fail)) i)
                 (λ (i) (when (eq? stage 'next) (bad)) (add1 i))
                 0 (λ (i) (when (eq? stage 'end) (amb-pause)) (< i 2)) #f #f))))
    (check-exn #rx"supplying alternatives"
               (λ () (observe (λ () (make-amb (λ () (sequence-generate* seq)))) amb-append 5)))))

(test-case "nested independent searches are allowed in suppliers and bodies"
  (same (prefix (in-amb (for/amb ([x (in-list (for/list ([n (in-amb (amb 1 2))]) n))]) x)) 10)
        '(1 2))
  (same (prefix (in-amb (for/amb ([x '(1 2)]
                                #:when (for/first ([ok (in-amb (amb #t #f))]) ok)) x)) 10)
        '(1 2))
  (same (prefix (in-amb (list (amb 1 2) (for/list ([x (in-amb (amb 'a 'b))]) x))) 10)
        '((1 (a b)) (2 (a b))))
  (check-exn #rx"source failed unchanged"
             (λ () (observe (λ () (make-amb (λ () (error "source failed unchanged"))))
                            amb-append 1))))

(test-case "pause observers yield ordinary zero/multiple-valued elements"
  (same (prefix (in-amb (begin (amb-pause) 42) #:on-pause (λ () 'paused)) 5) '(paused 42))
  (same (prefix (in-amb (begin (amb-pause) 42)) 5) '(42))
  (same (prefix (in-amb (list (void? (amb-pause)) (amb 1 2)) #:on-pause (λ () 'paused)) 5)
        '(paused (#t 1) (#t 2)))
  (same (for/list ([vs (sequence-map list
                                    (in-amb (begin (amb-pause) (values))
                                            #:on-pause (λ () (values))))]) vs) '(() ()))
  (same (for/list ([(x y) (in-amb (begin (amb-pause) (values 3 4))
                                 #:on-pause (λ () (values 1 2)))]) (+ x y)) '(3 7))
  (define n 0)
  (define seq (in-amb (make-amb (source '(3 (42))))
                      #:on-pause (λ () (set! n (add1 n)) 'P)))
  (same n 0)
  (same (prefix seq 2) '(P P))
  (same n 2)
  (check-exn #rx"observing a pause"
             (λ () (prefix (in-amb (amb-pause) #:on-pause (λ () (amb 1 2))) 1)))
  (same (prefix (in-amb (begin (amb-pause) 42)
                        #:on-pause (λ () (for/first ([v (in-amb (amb 'ok 'other))]) v))) 3)
        '(ok 42)))

(test-case "the missing call and eager-value counterexamples are executable"
  (define (thunks) (sequence-generate* (list (λ () 1) (λ () 2))))
  (same (for/list ([x (in-amb (make-amb thunks))]) (procedure? x)) '(#t #t))
  (same (prefix (in-amb ((make-amb thunks))) 5) '(1 2))
  (check-exn #rx"too early"
             (λ () (prefix (in-amb (make-amb (λ () (sequence-generate*
                                                   (list 1 (error "too early")))))) 1))))

(test-case "right-unit law is necessary, not an invisible singleton optimization"
  (define (extra-pause s t) (λ () (values 1 (amb-append s t))))
  (same (collect (extra-pause (source '((a))) (source '((b)))) 10)
        '(pause (answer (a)) (answer (b)) end))
  (same (observe (λ () (amb 'a 'b)) extra-pause 10)
        '(pause (answer (a)) pause (answer (b))))
  (for ([merge merges])
    (same (observe (λ () (make-amb (source '((a))))) merge 10)
          (observe (λ () 'a) merge 10))))

(test-case "sequence reflection does not silently promise a contextual inverse"
  (define (then x)
    (case x [(a) (amb-pause) 'A] [(b) 'B] [else 'C]))
  (same (observe (λ () (then (amb (amb 'a 'b) 'c))) amb-interleave 10)
        '(pause (answer (C)) (answer (B)) (answer (A))))
  (same (observe (λ () (then (make-amb
                             (λ () (sequence-generate* (in-amb (amb (amb 'a 'b) 'c)))))))
                 amb-interleave 10)
        '(pause (answer (B)) (answer (C)) (answer (A)))))

(printf "~a explicit comparisons; finite fuel; seed 20261114\n" comparisons)
