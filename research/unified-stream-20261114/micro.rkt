#lang racket/base
(require racket/list racket/runtime-path json)
(define-runtime-path producer "micro-producer.rkt")
(define mode (string->symbol (vector-ref (current-command-line-arguments) 0)))
(define n 2000000)
(define produce
  (dynamic-require producer
                   (case mode [(pair) 'pair-node] [(closure) 'closure-node]
                         [(thunk) 'thunk-node] [else 'values-node])))
(define stable-thunk ((dynamic-require producer 'closure-node) 1 2))
(define (work)
  (case mode
    [(pair)
     (for/fold ([sum 0]) ([i (in-range n)])
       (define p (produce i 2)) (+ sum (car p) (cdr p)))]
    [(values)
     (for/fold ([sum 0]) ([i (in-range n)])
       (define-values (r next) (produce i 2)) (+ sum r next))]
    [(closure)
     (for/fold ([sum 0]) ([i (in-range n)])
       (define-values (r next) ((produce i 2))) (+ sum r next))]
    [(thunk)
     (for/fold ([sum 0]) ([i (in-range n)]) (+ sum ((produce i 2)) 2))]
    [(reused-thunk)
     (for/fold ([sum 0]) ([i (in-range n)])
       (define-values (r next) (stable-thunk)) (+ sum r next))]))
(define expected (work))
(define samples
  (for/list ([_ (in-range 7)])
    (collect-garbage)
    (define before (current-memory-use 'cumulative))
    (define-values (vs cpu real gc) (time-apply work '()))
    (unless (= (car vs) expected) (error 'micro "checksum"))
    (hash 'cpu_ms cpu 'real_ms real 'gc_ms gc
          'allocated_bytes (- (current-memory-use 'cumulative) before))))
(define (stats key)
  (define xs (sort (map (λ (h) (hash-ref h key)) samples) <))
  (hash 'median (list-ref xs 3) 'min (car xs) 'max (last xs)))
(write-json (hash 'mode (symbol->string mode) 'n n 'checksum expected 'samples samples
                  'cpu_ms (stats 'cpu_ms) 'real_ms (stats 'real_ms)
                  'gc_ms (stats 'gc_ms) 'allocated_bytes (stats 'allocated_bytes)))
(newline)
