#lang racket/base

;; amb as installed (the flat frontier of private/amb.rkt) against the
;; kernel, and against the kernel with the same strategy written from its
;; equations (the merges of model.rkt), which is what the flat
;; strategies of the kernel replace.
;;
;;   master     amb as installed
;;   kernel     amb-append / amb-level of kernel.rkt
;;   equations  append-merge / level-merge of model.rkt
;;
;; Times are the median of 5 runs, in ms of CPU time.

(require (prefix-in m: amb)
         "../kernel.rkt"
         "../model.rkt")

(define (median xs) (list-ref (sort xs <) (quotient (length xs) 2)))

(define (time-of thk)
  (define result (thk))                 ; warm up
  (define times
    (for/list ([_ 5])
      (collect-garbage)
      (define-values (vs cpu real gc) (time-apply thk '()))
      cpu))
  (values (median times) result))

(define (pad s n) (string-append s (make-string (max 0 (- n (string-length s))) #\space)))
(define (lpad s n) (string-append (make-string (max 0 (- n (string-length s))) #\space) s))
(define (ratio a b) (if (zero? b) "-" (format "~ax" (/ (round (* 10.0 (/ a b))) 10.0))))

;; `master` and `kernel` are expressions with the same result; `kernel`
;; chooses with `amb`, by the strategy m.
(define-syntax-rule (bench name m master kernel)
  (let ()
    (define-values (tm rm) (time-of (λ () master)))
    (define-values (tk rk)
      (time-of (λ () (parameterize ([current-amb-merge (kernel-merge 'm)]) kernel))))
    (define-values (te re)
      (time-of (λ () (parameterize ([current-amb-merge (reference-merge 'm)]) kernel))))
    (unless (and (equal? rm rk) (equal? rm re))
      (error 'bench "~a: results differ" name))
    (printf "~a ~a ~a ~a ~a ~a\n"
            (pad name 38)
            (lpad (number->string tm) 7) (lpad (number->string tk) 7) (lpad (ratio tk tm) 7)
            (lpad (number->string te) 10) (lpad (ratio te tm) 7))))

;; The same without amb as installed: programs that pause by themselves.
(define-syntax-rule (bench/kernel name m kernel)
  (let ()
    (define-values (tk rk)
      (time-of (λ () (parameterize ([current-amb-merge (kernel-merge 'm)]) kernel))))
    (define-values (te re)
      (time-of (λ () (parameterize ([current-amb-merge (reference-merge 'm)]) kernel))))
    (unless (equal? rk re) (error 'bench "~a: results differ" name))
    (printf "~a ~a ~a ~a ~a ~a\n"
            (pad name 38)
            (lpad "" 7) (lpad (number->string tk) 7) (lpad "" 7)
            (lpad (number->string te) 10) (lpad (ratio te tk) 7))))

(define (header title)
  (printf "\n~a ~a ~a ~a ~a ~a\n" (pad title 38)
          (lpad "master" 7) (lpad "kernel" 7) (lpad "" 7) (lpad "equations" 10) (lpad "" 7)))


;; ---- depth-first search ----

(header "depth-first")

;; one choice point, many alternatives
(define N 200000)
(bench "wide: 200000 alternatives" append
       (for/sum ([x (m:in-amb/do (m:for/amb ([i (in-range N)]) i))]) x)
       (for/sum ([x (in-amb (amb-from (in-range N)))]) x))

;; Pythagorean triples
(define M 60)
(bench "triples below 60" append
       (for/list ([t (m:in-amb/do
                      (let* ([a (m:for/amb ([i (in-range 1 M)]) i)]
                             [b (m:for/amb ([i (in-range a M)]) i)]
                             [c (m:for/amb ([i (in-range b M)]) i)])
                        (unless (= (+ (* a a) (* b b)) (* c c)) (m:amb))
                        (list a b c)))])
         t)
       (for/list ([t (in-amb
                      (let* ([a (amb-from (in-range 1 M))]
                             [b (amb-from (in-range a M))]
                             [c (amb-from (in-range b M))])
                        (unless (= (+ (* a a) (* b b)) (* c c)) (amb))
                        (list a b c)))])
         t))

;; eight queens: all 92 solutions
(define (ok? q qs)
  (for/and ([p qs] [d (in-naturals 1)])
    (not (or (= p q) (= (abs (- p q)) d)))))
(define (m:queens n)
  (let loop ([k 0] [qs '()])
    (cond
      [(= k n) qs]
      [else
       (define q (m:for/amb #:length n ([i (in-range n)]) i))
       (unless (ok? q qs) (m:amb))
       (loop (add1 k) (cons q qs))])))
(define (queens n)
  (let loop ([k 0] [qs '()])
    (cond
      [(= k n) qs]
      [else
       (define q (amb-from (in-range n)))
       (unless (ok? q qs) (amb))
       (loop (add1 k) (cons q qs))])))
(bench "8 queens, all solutions" append
       (for/list ([s (m:in-amb/do (m:queens 8))]) s)
       (for/list ([s (in-amb (queens 8))]) s))

;; many choice points with two alternatives each, all open at once
(define (m:bits n) (if (zero? n) 0 (+ (m:amb 0 1) (* 2 (m:bits (sub1 n))))))
(define (bits n) (if (zero? n) 0 (+ (amb 0 1) (* 2 (bits (sub1 n))))))
(bench "deep: 2^18 leaves, 18 open choices" append
       (for/sum ([x (m:in-amb/do (m:bits 18))]) x)
       (for/sum ([x (in-amb (bits 18))]) x))

;; A choice in the first alternative of a choice, n times (issue #22):
;; every answer is under all the choices that are still open.
(define (m:left n) (if (zero? n) 0 (m:amb (m:left (sub1 n)) n)))
(define (left n) (if (zero? n) 0 (amb (left (sub1 n)) n)))
(for ([n '(4000 8000 16000)])
  (bench (format "left-nested, ~a deep" n) append
         (for/sum ([x (m:in-amb/do (m:left n))]) x)
         (for/sum ([x (in-amb (left n))]) x)))

;; the same shape, with the answers at the bottom: n answers under n
;; choices, each of which has one alternative left, which fails
(define (m:chain n) (if (zero? n) 0 (let ([x (m:amb 1 2)]) (if (= x 2) (m:amb) (+ x (m:chain (sub1 n)))))))
(define (chain n) (if (zero? n) 0 (let ([x (amb 1 2)]) (if (= x 2) (amb) (+ x (chain (sub1 n)))))))
(bench "chain of 8000 choices" append
       (for/list ([s (m:in-amb/do (m:chain 8000))]) s)
       (for/list ([s (in-amb (chain 8000))]) s))


;; ---- breadth-first search ----
;;
;; amb as installed with `current-amb-depth-first?` #f, and in the
;; kernel a pause at every choice, merged by amb-level
;; (tests/master-flags.rkt).

(header "breadth-first")

(define-syntax-rule (m:breadth-first e)
  (parameterize ([m:current-amb-depth-first? #f]) e))
(define-syntax-rule (amb/bfs e ...)
  (begin (amb-pause) (amb e ...)))

(define (bits/bfs n) (if (zero? n) 0 (+ (amb/bfs 0 1) (* 2 (bits/bfs (sub1 n))))))
(bench "deep: 2^14 leaves" level
       (m:breadth-first (for/sum ([x (m:in-amb/do (m:bits 14))]) x))
       (for/sum ([x (in-amb (bits/bfs 14))]) x))


;; With a pause at every choice, a choice in the first alternative of a
;; choice is not open when the answers come.  Here it is: n streams that
;; stay open for m rounds, under n choices, and in every round each
;; stream has one answer.  These programs pause by themselves, so there
;; is no column for amb as installed; the last column is the ratio to
;; the kernel.
(define (tick m) (if (zero? m) 0 (amb m (begin (amb-pause) (tick (sub1 m))))))
(define (rows n m) (if (zero? n) (tick m) (amb (rows (sub1 n) m) (tick m))))
(for ([n '(250 500 1000)])
  (bench/kernel (format "~a streams, 20 rounds" n) level
                (for/sum ([x (in-amb (rows n 20))]) x)))
