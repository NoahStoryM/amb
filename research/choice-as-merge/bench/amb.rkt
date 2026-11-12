#lang racket/base

;; Depth-first search: amb as installed (the flat frontier of
;; private/amb.rkt) against the kernel with amb-append.  The programs
;; have no pauses, so this measures what choosing by merging streams
;; costs where a stack of choice points would do.

(require (prefix-in m: amb)
         "../kernel.rkt")

(define (median xs) (list-ref (sort xs <) (quotient (length xs) 2)))

(define-syntax-rule (bench name master kernel)
  (let ()
    (define (time-of thk)
      (define result (thk))               ; warm up
      (define times
        (for/list ([_ 5])
          (collect-garbage)
          (define-values (vs cpu real gc) (time-apply thk '()))
          cpu))
      (values (median times) result))
    (define-values (tm rm) (time-of (λ () master)))
    (define-values (tk rk) (time-of (λ () kernel)))
    (unless (equal? rm rk) (error 'bench "~a: results differ: ~e ~e" name rm rk))
    (printf "~a  master ~a ms   kernel ~a ms   (~ax)\n"
            (let ([s name]) (string-append s (make-string (max 0 (- 34 (string-length s))) #\space)))
            tm tk (if (zero? tm) '- (/ (round (* 10.0 (/ tk tm))) 10.0)))))

;; one choice point, many alternatives
(define N 200000)
(bench "wide: 200000 alternatives"
       (for/sum ([x (m:in-amb/do (m:for/amb ([i (in-range N)]) i))]) x)
       (for/sum ([x (in-amb (amb-from (in-range N)))]) x))

;; many choice points with two alternatives each, all live at once
(define (m:bits n) (if (zero? n) 0 (+ (m:amb 0 1) (* 2 (m:bits (sub1 n))))))
(define (bits n) (if (zero? n) 0 (+ (amb 0 1) (* 2 (bits (sub1 n))))))
(bench "deep: 2^16 leaves, 16 live choices"
       (for/sum ([x (m:in-amb/do (m:bits 16))]) x)
       (for/sum ([x (in-amb (bits 16))]) x))

;; Pythagorean triples
(define M 60)
(bench "triples below 60"
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
(bench "8 queens, all solutions"
       (for/list ([s (m:in-amb/do (m:queens 8))]) s)
       (for/list ([s (in-amb (queens 8))]) s))

;; a long chain of choices, each with one alternative left: an answer
;; passes through every merge on its path
(define (m:chain n) (if (zero? n) 0 (let ([x (m:amb 1 2)]) (if (= x 2) (m:amb) (+ x (m:chain (sub1 n)))))))
(define (chain n) (if (zero? n) 0 (let ([x (amb 1 2)]) (if (= x 2) (amb) (+ x (chain (sub1 n)))))))
(bench "chain of 2000 choices"
       (for/list ([s (m:in-amb/do (m:chain 2000))]) s)
       (for/list ([s (in-amb (chain 2000))]) s))
(bench "chain of 4000 choices"
       (for/list ([s (m:in-amb/do (m:chain 4000))]) s)
       (for/list ([s (in-amb (chain 4000))]) s))
