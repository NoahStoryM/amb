#lang racket/base

;; Run with: racket pkgs/amb/tests/bench-observe.rkt CASE [BATCHES] [ROUNDS]
;; Each round reports CPU, wall, and GC milliseconds and estimated allocated bytes.

(module+ main
  (require racket/cmdline
           racket/runtime-path
           racket/stream
           "../main.rkt"
           (only-in "../private/data/sequence.rkt" make-sequence)
           (only-in "../private/generator.rkt" end))

  (define-runtime-path observe-path "../private/observe.rkt")
  (define make-observer-sequence
    (parameterize ([current-namespace (module->namespace observe-path)])
      (define advance (namespace-variable-value 'next-answer #t (λ () #f)))
      (if advance
          (λ (gen) (make-sequence gen advance))
          (let ([elements (namespace-variable-value 'elements)])
            (λ (gen) (make-sequence (elements gen #f)))))))

  (define answers (expt 4 7))

  ;; A preexisting answer list and a generator that returns itself isolate
  ;; the observer from search and per-answer allocation in the producer.
  (define (observer)
    (define left answers)
    (define (gen)
      (if (zero? left)
          (end)
          (begin (set! left (sub1 left)) (values '(1) gen))))
    (make-observer-sequence gen))

  (define (flat-search)
    (for/amb #:length answers ([i (in-range answers)]) i))

  (define (nested-search)
    (let loop ([depth 7] [i 0])
      (if (zero? depth)
          i
          (loop (sub1 depth) (+ (* 4 i) (amb 0 1 2 3))))))

  (define-syntax-rule (consumer seq)
    (λ (ordered?)
      (for/fold ([count 0] [sum 0]) ([x seq])
        (when (and ordered? (not (= x count)))
          (error 'bench-observe "answer ~a at position ~a" x count))
        (values (add1 count) (+ sum x)))))

  (define-values (scenario batches rounds)
    (command-line
     #:args (scenario [batches "64"] [rounds "1"])
     (values scenario (string->number batches) (string->number rounds))))

  (define consume
    (case scenario
      [("observer") (consumer (observer))]
      [("flat-do") (consumer (in-amb*/do flat-search))]
      [("flat-for") (consumer (in-amb (flat-search)))]
      [("nested-do") (consumer (in-amb*/do nested-search))]
      [("nested-for") (consumer (in-amb (nested-search)))]
      [("cached") (consumer (in-stream (in-amb* flat-search)))]
      [else (error 'bench-observe "unknown case ~a" scenario)]))

  (define observer? (equal? scenario "observer"))
  (define expected-sum (if observer? answers (quotient (* answers (sub1 answers)) 2)))
  (define (check count sum)
    (unless (and (= count answers) (= sum expected-sum))
      (error 'bench-observe "unexpected count ~a or sum ~a" count sum)))

  (for ([_ (in-range 4)])
    (define-values (count sum) (consume (not observer?)))
    (check count sum))

  (for ([_ (in-range rounds)])
    (collect-garbage)
    (define before (current-memory-use 'cumulative))
    (define-values (_ cpu wall gc)
      (time-apply (λ ()
                    (for ([_ (in-range batches)])
                      (define-values (count sum) (consume #f))
                      (check count sum)))
                  '()))
    (define allocated (- (current-memory-use 'cumulative) before))
    (printf "~a,~a,~a,~a,~a,~a\n" scenario (* batches answers) cpu wall gc allocated)))
