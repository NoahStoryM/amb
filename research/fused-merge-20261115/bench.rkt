#lang racket/base

;; The equations of demo.rkt and the flat mergers of merge.rkt, on the
;; two programs that are quadratic with the equations.  Milliseconds,
;; the median of five runs.

(require "../minimal-reference-20261114/demo.rkt"
         (prefix-in flat: "merge.rkt"))

(define-syntax-rule (ms e)
  (let ([ts (for/list ([_ 5])
              (collect-garbage)
              (define t (current-inexact-milliseconds))
              e
              (- (current-inexact-milliseconds) t))])
    (inexact->exact (round (list-ref (sort ts <) 2)))))

(define (count-answers thk)
  (for/fold ([c 0]) ([x (in-amb/do (thk))]) (add1 c)))

;; Claude's example (#22)
(define (left n) (if (= n 0) (amb 0) (amb (left (sub1 n)) n)))

;; a stream that is merged again after each of its events
(define (again merge n)
  (define (leaf i) (λ () (values (list i) (if (zero? i) (λ () (values #f void)) (leaf (sub1 i))))))
  (define (nothing) (values #f void))
  (for/fold ([g (leaf n)]) ([_ n])
    (define-values (v next) ((merge g nothing)))
    next))

;; other programs, where there is nothing to flatten
(define (wide n) (for/amb ([i n]) i))
(define (ok? q qs)
  (for/and ([p qs] [d (in-naturals 1)])
    (not (or (= p q) (= (abs (- p q)) d)))))
(define (queens n)
  (let loop ([k 0] [qs '()])
    (if (= k n)
        qs
        (let ([q (for/amb ([i n]) i)])
          (unless (ok? q qs) (amb))
          (loop (add1 k) (cons q qs))))))
(define (deep d) (if (zero? d) 0 (+ (amb 0 1) (deep (sub1 d)))))
(define (deep/pause d) (if (zero? d) 0 (begin (amb-pause) (+ (amb 0 1) (deep/pause (sub1 d))))))

(define (row label spec flat thk)
  (define a (parameterize ([current-amb-merger spec]) (ms (thk))))
  (define b (parameterize ([current-amb-merger flat]) (ms (thk))))
  (printf "~a ~a ~a\n" (pad label 44) (pad a 9) (pad b 9)))
(define (pad x n)
  (define s (format "~a" x))
  (string-append s (make-string (max 0 (- n (string-length s))) #\space)))

(printf "~a ~a ~a\n" (pad "" 44) (pad "equations" 9) (pad "flat" 9))
(for ([n '(1000 2000 4000 8000 16000)])
  (row (format "append: nested on the left, ~a deep" n) amb-append flat:amb-append
       (λ () (count-answers (λ () (left n))))))
(for ([n '(1000 2000 4000)])
  (row (format "level: nested on the left, ~a deep" n) amb-level flat:amb-level
       (λ () (count-answers (λ () (left n))))))
(for ([n '(5000 10000 20000)])
  (row (format "append: merged again and again, ~a" n) amb-append flat:amb-append
       (λ () ((again (current-amb-merger) n)))))
(for ([n '(5000 10000 20000)])
  (row (format "level: merged again and again, ~a" n) amb-level flat:amb-level
       (λ () ((again (current-amb-merger) n)))))
(row "append: wide, 200000 alternatives" amb-append flat:amb-append
     (λ () (count-answers (λ () (wide 200000)))))
(row "append: 10 queens" amb-append flat:amb-append
     (λ () (count-answers (λ () (queens 10)))))
(row "append: deep, 2^18 leaves" amb-append flat:amb-append
     (λ () (count-answers (λ () (deep 18)))))
(row "level: deep, 2^14 leaves, with pauses" amb-level flat:amb-level
     (λ () (count-answers (λ () (deep/pause 14)))))
