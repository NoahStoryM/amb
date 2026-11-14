#lang racket/base
(require rackunit racket/list racket/sequence racket/port racket/match
         "support.rkt" (prefix-in model: "baseline/model.rkt")
         (prefix-in c: "c.rkt") (prefix-in d: "d.rkt"))

(define apis (map load-api '(A B C D)))
(define ms '(append interleave alternate level))
(define comparisons 0)
(define (same got expected)
  (set! comparisons (add1 comparisons))
  (check-equal? got expected))

(test-case "random direct goals, independent explicit-CPS oracle, and side effects"
  (random-seed 20261114)
  (for* ([m ms] [i (in-range 900)])
    (define g (model:random-goal (add1 (random 18)) '(seq alt alt* local once ifte)))
    (define traces
      (for/list ([a apis])
        (define log '())
        (parameterize ([(op a 'current-amb-merge) (merge-of a m)])
          (observe a ((op a 'amb-reify)
                      (λ () (run-goal a g '(start) (λ (x) (set! log (cons x log))))))
                   300 (λ () log)))))
    (define expected (car traces))
    (for ([trace (cdr traces)]) (same trace expected))
    (define oracle ((model:spec m) g))
    (define normalized (map (λ (e) (if (eq? e 'pause) 'pause (list e))) oracle))
    (same (map car expected) (take (append normalized '(end))
                                 (min 300 (add1 (length normalized)))))))

(test-case "zero/multiple values, finite and infinite prefixes, local and once"
  (for ([m ms])
    (define all
      (for/list ([a apis])
        (define choose (op a 'amb*))
        (define pause (op a 'amb-pause))
        (define fail (op a 'amb-fail))
        (define (never) (pause) (never))
        (define (count n)
          (choose (merge-of a m)
                  (vector (λ () n) (λ () (pause) (count (add1 n))))))
        (define (multi)
          (choose (merge-of a m)
                  (vector (λ () (values)) (λ () (values 1 2))
                          (λ () (pause) (values #f 'pause)) fail)))
        (define (once-never)
          (choose (merge-of a m)
                  (vector (λ () ((op a 'amb-once) never)) (λ () 'reachable))))
        (parameterize ([(op a 'current-amb-merge) (merge-of a m)])
          (for/list ([thk (list multi never (λ () (count 0)) once-never)])
            (events a ((op a 'amb-reify) thk) 40)))))
    (for ([result (cdr all)]) (same result (car all)))))

(test-case "large depth, width, failures and lazy amb-from"
  (for ([m ms])
    (define all
      (for/list ([a apis])
        (define choose (op a 'amb*))
        (define (left n)
          (if (zero? n) 0
              (choose (merge-of a m) (vector (λ () (left (sub1 n))) (λ () n)))))
        (define (right n)
          (if (zero? n) 0
              (choose (merge-of a m) (vector (λ () n) (λ () (right (sub1 n)))))))
        (define (wide)
          (choose (merge-of a m)
                  (for/vector ([i (in-range 200)])
                    (λ () (if (zero? (modulo i 7)) i ((op a 'amb-fail)))))))
        (parameterize ([(op a 'current-amb-merge) (merge-of a m)])
          (for/list ([thk (list (λ () (left 120)) (λ () (right 120)) wide
                               (λ () ((op a 'amb-from) (in-naturals))))])
            (events a ((op a 'amb-reify) thk) 300)))))
    (for ([result (cdr all)]) (same result (car all)))))

(test-case "pure stream expressions, mixed strategies and residual remerging"
  (define (gen n)
    (if (<= n 1)
        `(leaf ,(for/list ([_ (in-range (random 7))])
                  (if (zero? (random 2)) 'pause (list (random 20)))))
        (let ([l (add1 (random (sub1 n)))])
          `(,(list-ref ms (random 4)) ,(gen l) ,(gen (- n l))))))
  (define (ev a g)
    (match g
      [`(leaf ,es) (raw-stream a es)]
      [`(,m ,s ,t) (merge-raw a m (ev a s) (if (api-new? a) (ev a t) (λ () (ev a t))))]))
  (for ([_ (in-range 1800)])
    (define g (gen (add1 (random 12))))
    (define all (map (λ (a) (events a (ev a g) 120)) apis))
    (for ([result (cdr all)]) (same result (car all))))
  ;; Force one answer, remerge the residual, repeat. Exercise fusion after use.
  (for ([m '(append level)])
    (define all
      (for/list ([a apis])
        (define s (raw-stream a (build-list 201 list)))
        (for ([_ (in-range 200)])
          (define merged (merge-raw a m s (if (api-new? a) (stream-empty a) (λ () '()))))
          (if (api-new? a)
              (let-values ([(r next) (merged)]) (set! s next))
              (set! s ((cdr merged)))))
        (events a s 10)))
    (for ([result (cdr all)]) (same result (car all)))))

(test-case "native sequence-generate* streams need no per-element adapter"
  (for ([a (cddr apis)])
    (for ([seq (list (in-naturals) (in-range 100) (in-list '(a b c))
                     (in-parallel (in-range 5) (in-list '(a b c d e))))])
      (define expected (for/list ([_ (in-range 8)]
                                 [vs (sequence-map list seq)]) vs))
      (same (filter list? (events a (λ () (sequence-generate* seq)) 8)) expected))
    (for ([m ms])
      (define native (λ () (sequence-generate* (in-range 3))))
      (define own ((op a 'amb-reify) (λ () ((op a 'amb-pause)) 'own)))
      (same (sort (filter list? (events a ((merge-of a m) native own) 30))
                  string<? #:key (λ (x) (format "~a" x)))
            (sort '((0) (1) (2) (own)) string<? #:key (λ (x) (format "~a" x)))))
    ;; Single-pass source: the first read happens at first advance only.
    (define p (open-input-string "abc"))
    (define s (λ () (sequence-generate* (in-port read-char p))))
    (same (peek-char p) #\a)
    (same (events a s 1) '((#\a)))
    (same (peek-char p) #\b)
    ;; End tail errors if called: correctly recognizing #f avoids it.
    (same (events a (λ () (sequence-generate* '())) 10) '(end))
    (define-values (r end) (sequence-generate* '()))
    (check-exn exn:fail:contract? end)
    (define zero-seq
      (make-do-sequence (λ () (values (λ (_) (values)) add1 0 (λ (i) (< i 3)) #f #f))))
    (same (events a (λ () (sequence-generate* zero-seq)) 10) '(() () () end))
    (same (events a ((op a 'amb-reify) (λ () ((op a 'amb-from) zero-seq))) 10)
          '(() () () end))))

(test-case "reverse sequence conversion and fuel boundaries"
  (for ([a (cddr apis)])
    (define (bomb) (error "tail forced"))
    (same (events a (λ () (values '(a) bomb)) 1) '((a)))
    (same (events a (λ () (values 1 bomb)) 1) '(pause))
    (same (events a bomb 0) '())
    (same (for/list ([(x y) ((op a 'in-answers)
                             (raw-stream a '(pause (1 2) pause (3 4)) ))]) (+ x y)) '(3 7))
    (same (for/list ([e ((op a 'in-amb-events)
                         (raw-stream a '(() pause (#f pause))))]) e)
          '(() pause (#f pause)))
    (same ((op a 'amb-take) 1 (λ () (values '(a) bomb))) '(a))
    ;; Advancing a sequence must not read a tail beyond a requested prefix.
    (same (for/list ([_ (in-range 1)] [x ((op a 'in-answers) (λ () (values '(a) bomb)))]) x)
          '(a))
    (define seq ((op a 'in-answers) ((op a 'unit) '(a))))
    (same (sequence->list seq) '(a))
    (check-exn #rx"single-pass" (λ () (sequence->list seq)))))

(test-case "choice macros, lazy source boundaries, shuffler and multi-value merging"
  (same (c:amb-events (c:amb-reify (λ () (c:for*/amb ([i 3] [j i]) (list i j)))) 10)
        '((1 0) (2 0) (2 1)))
  (same (d:amb-events (d:amb-reify (λ () (d:for/amb #:length 5 ([i 3]) i))) 10)
        '(0 1 2))
  (for ([a (cddr apis)])
    (define (bad-seq)
      (make-do-sequence
       (λ () (values values add1 0
                     (λ (i) (or (zero? i) (error "asked for more"))) #f #f))))
    (same (events a ((op a 'amb-reify) (λ () ((op a 'amb-from) (bad-seq)))) 1) '((0)))
    (check-exn #rx"asked for more"
               (λ () (events a ((op a 'amb-reify)
                                 (λ () ((op a 'amb-from) (bad-seq)))) 2)))
    (define native (λ () (sequence-generate*
                         (in-parallel (in-range 3) (in-list '(a b c))))))
    (for ([m ms])
      (same (sort (filter list? (events a ((merge-of a m) native (unit a '(own 0))) 20))
                  string<? #:key (λ (x) (format "~a" x)))
            '((0 a) (1 b) (2 c) (own 0))))
    (define shuffled '())
    (define s
      ((op a 'amb-reify)
       (λ ()
         (parameterize ([(op a 'current-amb-shuffler)
                         (λ (v) (set! shuffled (cons (vector-length v) shuffled))
                           (define temp (vector-ref v 0))
                           (vector-set! v 0 (vector-ref v 1))
                           (vector-set! v 1 temp))])
           ((op a 'amb*) (merge-of a 'append) (vector (λ () 'a) (λ () 'b)))))))
    (same (events a s 10) '((b) (a) end))
    (same shuffled '(2))))

(test-case "reflection preserves zero values, multiple values and pause identity"
  (for* ([a apis] [m ms])
    (define es '(() pause (1 2) (#f pause) pause (last)))
    (parameterize ([(op a 'current-amb-merge) (merge-of a m)])
      (define s ((op a 'amb-reify)
                 (λ () ((op a 'amb-reflect) (raw-stream a es)))))
      (same (events a s 30) (append es '(end)))))
  (for ([a (cddr apis)])
    (define initiated 0)
    (define seq
      (make-do-sequence
       (λ () (set! initiated (add1 initiated))
         (values values add1 0 (λ (i) (< i 3)) #f #f))))
    (define s ((merge-of a 'append) (λ () (sequence-generate* seq)) (unit a '(tail))))
    (same initiated 0)
    (same (events a s 30) '((0) (1) (2) (tail) end))
    (same initiated 1)))

(test-case "pause counts preserve each logical event and every scheduling turn"
  (for* ([m ms] [n (in-range 1 9)] [k (in-range 1 9)])
    (define c (list-ref apis 2))
    (define d (list-ref apis 3))
    (define (build a)
      ((merge-of a m) ((op a 'pause-stream) n (unit a '(left)))
                       ((op a 'pause-stream) k (unit a '(right)))))
    (for ([fuel '(0 1 2 3 5 10 30)])
      (same (events d (build d) fuel) (events c (build c) fuel))))
  (define-values (r next)
    ((d:amb-level (d:pause-stream 5 (d:unit '(a)))
                  (d:pause-stream 3 (d:unit '(b))))))
  (same r 3)
  (same (events (list-ref apis 3) next 20) '((b) pause pause (a) end))
  ;; Count fusion must not invoke an opaque tail to discover more pauses.
  (define ran 0)
  (define merged
    (d:amb-level (d:pause-stream 3 (λ () (set! ran (add1 ran)) (values '(a) d:done)))
                  (d:pause-stream 3 (d:unit '(b)))))
  (same (events (list-ref apis 3) merged 3) '(pause pause pause))
  (same ran 0)
  (same (events (list-ref apis 3) (d:pause-stream 1000000000 (d:unit '(x))) 4)
        '(pause pause pause pause)))

(test-case "compressed pauses under nested mixed strategies"
  (define ca (list-ref apis 2))
  (define da (list-ref apis 3))
  (define (gen n)
    (if (<= n 1)
        `(leaf ,(for/list ([_ (in-range (random 12))])
                  (if (< (random 4) 3) 'pause (list (random 20)))))
        (let ([l (add1 (random (sub1 n)))])
          `(,(list-ref ms (random 4)) ,(gen l) ,(gen (- n l))))))
  (define (compressed es)
    (cond
      [(null? es) d:done]
      [(eq? (car es) 'pause)
       (let loop ([xs es] [n 0])
         (if (and (pair? xs) (eq? (car xs) 'pause))
             (loop (cdr xs) (add1 n))
             (d:pause-stream n (compressed xs))))]
      [else (λ () (values (car es) (compressed (cdr es))))]))
  (define (ev a g)
    (match g
      [`(leaf ,es) (if (eq? (api-name a) 'D) (compressed es) (raw-stream a es))]
      [`(,m ,s ,t) ((merge-of a m) (ev a s) (ev a t))]))
  (for ([_ (in-range 1800)])
    (define g (gen (add1 (random 12))))
    (for ([fuel '(1 3 9 90)])
      (same (events da (ev da g) fuel) (events ca (ev ca g) fuel)))))

(test-case "explicit startup change, parameter extent and dynamic-wind"
  (for ([a apis])
    (define ran 0)
    (define s ((op a 'amb-reify) (λ () (set! ran (add1 ran)) 7)))
    (same ran (if (api-new? a) 0 1))
    (same (events a s 1) '((7)))
    (same ran 1)
    (define p (make-parameter 'outer))
    (define t (parameterize ([p 'creation]) ((op a 'amb-reify) (λ () (p)))))
    (same (parameterize ([p 'consumption]) (events a t 2))
          (list (list (if (api-new? a) 'consumption 'creation)) 'end))
    (define log '())
    (define (log! x) (set! log (cons x log)))
    (define w ((op a 'amb-reify)
               (λ () (dynamic-wind (λ () (log! 'enter))
                                   (λ () ((op a 'amb-pause)) (log! 'body) 'x)
                                   (λ () (log! 'exit))))))
    (same log (if (api-new? a) '() '(exit enter)))
    (same (events a w 2) '(pause (x)))
    (same log '(exit body enter exit enter)))
  (define ran 0)
  (define s (c:amb-reify/eager (λ () (set! ran 1) 'x)))
  (same ran 1)
  (same (c:amb-take 1 s) '(x)))

(printf "~a explicit comparisons completed; deterministic seed 20261114\n" comparisons)
