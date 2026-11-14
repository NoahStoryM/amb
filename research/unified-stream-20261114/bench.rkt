#lang racket/base
;; One isolated workload per process. Parent runner enforces a hard timeout.
(require racket/list racket/sequence racket/match json "support.rkt")
(define argv (current-command-line-arguments))
(define a (load-api (string->symbol (vector-ref argv 0))))
(define workload (string->symbol (vector-ref argv 1)))
(define n (string->number (vector-ref argv 2)))
(define new? (api-new? a))
(define A? (eq? (api-name a) 'A))
(define reify (op a 'amb-reify))
(define choose (op a 'amb*))
(define pause (op a 'amb-pause))
(define fail (op a 'amb-fail))
(define from (op a 'amb-from))
(define answers (op a 'in-answers))
(define parameter (op a 'current-amb-merge))
(define append-merge (merge-of a 'append))
(define interleave (merge-of a 'interleave))
(define level (merge-of a 'level))
(define terminal (if new? (op a 'done) (λ () '())))
(define (merge m s t)
  (m (if (or A? new?) s (λ () s)) (if new? t (λ () t))))

;; Native drains, with no test-driver representation adapter and no output list.
;; Result = #(answers pauses checksum), including compressed logical pauses.
(define (drain s [fuel +inf.0])
  (if new?
      (let loop ([s s] [f fuel] [ans 0] [ps 0] [sum 0])
        (cond
          [(<= f 0) (vector ans ps sum)]
          [else
           (define-values (r next) (s))
           (cond
             [(not r) (vector ans ps sum)]
             [(list? r) (loop next (sub1 f) (add1 ans) ps (+ sum (car r)))]
             ;; min with +inf.0 coerces r to an inexact number, boxing the
             ;; logical pause counter on every event. Preserve the selected
             ;; exact operand so this observer does not bias C/D allocation.
             [else (define k (if (< f r) f r))
              (loop next (- f k) ans (+ ps k) sum)])]))
      (let loop ([s s] [f fuel] [ans 0] [ps 0] [sum 0])
        (cond
          [(or (<= f 0) (null? s)) (vector ans ps sum)]
          [(pair? s)
           (if (<= f 1) (vector (add1 ans) ps (+ sum (caar s)))
               (loop ((cdr s)) (sub1 f) (add1 ans) ps (+ sum (caar s))))]
          [else
           (if (<= f 1) (vector ans (add1 ps) sum)
               (loop (s) (sub1 f) ans (add1 ps) sum))]))))

(define (source n mode)
  (if new?
      (let loop ([i 0])
        (λ ()
          (cond
            [(= i n) (terminal)]
            [(or (eq? mode 'pauses) (and (eq? mode 'mixed) (odd? i)))
             (values 1 (loop (add1 i)))]
            [else (values (list i) (loop (add1 i)))])))
      (let loop ([i 0])
        (cond
          [(= i n) '()]
          [(or (eq? mode 'pauses) (and (eq? mode 'mixed) (odd? i)))
           (λ () (loop (add1 i)))]
          [else (cons (list i) (λ () (loop (add1 i))))]))))

(define (left n) (if (zero? n) 0
                     (choose append-merge (vector (λ () (left (sub1 n))) (λ () n)))))
(define (right n) (if (zero? n) 0
                      (choose append-merge (vector (λ () n) (λ () (right (sub1 n)))))))
(define (wide failures?)
  (choose append-merge
          (for/vector ([i (in-range n)])
            (λ () (if (and failures? (not (zero? (modulo i 100)))) (fail) i)))))

;; A/B must allocate one old node per native sequence element. C/D use the
;; exact sequence-generate* result, with no translation to another node type.
(define (native)
  (if new?
      (λ () (sequence-generate* (in-range n)))
      (let-values ([(r next) (sequence-generate* (in-range n))])
        (let convert ([r r] [next next])
          (if r (cons r (λ () (let-values ([(r next) (next)]) (convert r next)))) '())))))

(define (fusion)
  (define s (source (add1 n) 'answers))
  (for ([_ (in-range n)])
    (define t (merge append-merge s (if new? terminal '())))
    (if new?
        (let-values ([(r next) (t)]) (set! s next))
        (set! s ((cdr t)))))
  (drain s))

(define (counting k)
  (choose (parameter) (vector (λ () k) (λ () (pause) (counting (add1 k))))))
(define (rows width rounds)
  (define (row k)
    (if (zero? k) 0
        (choose level (vector (λ () k) (λ () (pause) (row (sub1 k)))))))
  (if (zero? width) (row rounds)
      (choose level (vector (λ () (rows (sub1 width) rounds)) (λ () (row rounds))))))

(define (work)
  (parameterize ([parameter append-merge])
    (case workload
      [(answers pauses mixed) (drain (source n workload))]
      [(left) (drain (reify (λ () (left n))))]
      [(right) (drain (reify (λ () (right n))))]
      [(wide) (drain (reify (λ () (wide #f))))]
      [(failures) (drain (reify (λ () (wide #t))))]
      [(fusion) (fusion)]
      [(interleave)
       (parameterize ([parameter interleave])
         (drain (reify (λ () (choose interleave
                                    (vector (λ () (counting 0)) (λ () (counting 0)))))) n))]
      [(level)
       (parameterize ([parameter level]) (drain (reify (λ () (rows n 30)))))]
      [(native) (drain (native))]
      [(roundtrip)
       (define sum (for/sum ([v (answers (reify (λ () (from (in-range n)))))]) v))
       (vector n 0 sum)]
      [(counted)
       ;; Same logical P^n trace; C expands P one by one; D can return a block.
       (drain (if new?
                  ((op a 'pause-stream) n (unit a '(0)))
                  (let loop ([i n]) (if (zero? i) (unit a '(0))
                                       (λ () (loop (sub1 i)))))))]
      [else (error 'bench "unknown workload ~e" workload)])))

(define expected (work)) ; warmup and checksum
;; A single calibration can catch a GC or scheduler delay. Use three bounded
;; warm calibrations and their CPU median, rather than trusting one wall time.
(define calibration
  (for/list ([_ (in-range 3)])
    (define-values (vs cpu real gc) (time-apply work '()))
    (unless (equal? (car vs) expected) (error 'bench "unstable result"))
    cpu))
(define initial-ms (list-ref (sort calibration <) 1))
;; Target 100 ms, cap at 400 repetitions; no adaptive unbounded loop.
(define repetitions (max 1 (min 400 (inexact->exact (ceiling (/ 100 (max 0.25 initial-ms)))))))
(define samples
  (for/list ([_ (in-range 7)])
    (collect-garbage)
    (define allocation0 (current-memory-use 'cumulative))
    (define-values (vs cpu real gc)
      (time-apply (λ ()
                    (for ([_ (in-range repetitions)])
                      (unless (equal? (work) expected) (error 'bench "result changed")))) '()))
    (define allocated (- (current-memory-use 'cumulative) allocation0))
    (hash 'cpu_ms (/ cpu (* 1.0 repetitions)) 'real_ms (/ real (* 1.0 repetitions))
          'gc_ms (/ gc (* 1.0 repetitions)) 'allocated_bytes (/ allocated (* 1.0 repetitions)))))
(define (stats key)
  (define xs (sort (map (λ (h) (hash-ref h key)) samples) <))
  (hash 'median (list-ref xs 3) 'min (car xs) 'max (last xs)))
(write-json
 (hash 'variant (symbol->string (api-name a)) 'workload (symbol->string workload)
       'n n 'repetitions repetitions 'result (vector->list expected) 'samples samples
       'cpu_ms (stats 'cpu_ms) 'real_ms (stats 'real_ms) 'gc_ms (stats 'gc_ms)
       'allocated_bytes (stats 'allocated_bytes) 'racket (version)
       'vm (symbol->string (system-type 'vm))))
(newline)
