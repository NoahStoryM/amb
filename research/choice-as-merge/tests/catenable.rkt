#lang racket/base

;; catenable.rkt against plain lists, and the steps it takes.

(require rackunit
         "../catenable.rkt")

(define (list->cat l)
  (for/fold ([a '()]) ([x l]) (cat-append a (cat-of x))))

(test-case "the lists are lists"
  (check-equal? (cat->list '()) '())
  (check-equal? (cat->list (cat-of 1)) '(1))
  (check-equal? (cat->list (list->cat '(1 2 3 4 5))) '(1 2 3 4 5))
  (check-equal? (cat->list (cat-append (list->cat '(1 2 3)) (list->cat '(4 5)))) '(1 2 3 4 5))
  (check-equal? (cat->list (cat-append '() (cat-of 1))) '(1))
  (check-equal? (cat->list (cat-append (cat-of 1) '())) '(1))
  (check-equal? (cat-first (cat-rest (list->cat '(1 2 3)))) 2))

;; Random operations on a pool of lists, each kept beside the plain list
;; it should be.  With `once?`, a list that an operation has used is
;; taken out of the pool, as in a single pass; without, it stays and may
;; be used again.  Returns the number of operations.
(define (random-operations n once?)
  (define pool (make-vector 8 (cons '() '())))      ; (cons cat list)
  (define fresh 0)
  (define (take! i)
    (begin0 (vector-ref pool i)
            (when once? (vector-set! pool i (cons '() '())))))
  (for ([_ n])
    (define i (random 8))
    (define j (random 8))
    (case (random 4)
      [(0)                              ; a new element at the end
       (define a (take! i))
       (set! fresh (add1 fresh))
       (vector-set! pool i (cons (cat-append (car a) (cat-of fresh))
                                 (append (cdr a) (list fresh))))]
      [(1)                              ; append two lists
       (unless (and once? (= i j))
         (define a (take! i))
         (define b (take! j))
         (vector-set! pool i (cons (cat-append (car a) (car b))
                                   (append (cdr a) (cdr b)))))]
      [else                             ; remove the first element
       (define a (take! i))
       (unless (null? (cdr a))
         (check-equal? (cat-first (car a)) (cadr a))
         (vector-set! pool i (cons (cat-rest (car a)) (cddr a))))]))
  (for ([a pool])
    (check-equal? (cat->list (car a)) (cdr a)))
  n)

(random-seed 20261112)

(test-case "random operations, every list used once"
  (for ([_ 200]) (random-operations 300 #t)))

(test-case "random operations, lists used again"
  ;; the elements do not depend on a list being used once
  (for ([_ 200]) (random-operations 300 #f)))

;; ---- steps ----

(define-syntax-rule (steps-of e)
  (let ([before (cat-steps)])
    e
    (- (cat-steps) before)))

(test-case "constant amortized time, every list used once"
  ;; `cat-rest` counts a step for each link and for each element of a
  ;; queue that it reverses.  By the theorem, the steps are at most a
  ;; constant times the number of operations; here the constant is 3.
  (define-syntax-rule (check-steps operations e)
    (let ([steps (steps-of e)])
      (check-true (<= steps (* 3 operations))
                  (format "~a steps for ~a operations" steps operations))))
  (for ([_ 200])
    (check-steps 2000 (random-operations 2000 #t)))
  (for ([n '(1000 10000 100000)])
    ;; n elements added at the end one by one, and then removed
    (check-steps (* 2 n)
                 (let loop ([a (list->cat (build-list n values))])
                   (unless (null? a) (loop (cat-rest a)))))
    ;; n elements, each put in front of the others, and then removed
    (check-steps (* 2 n)
                 (let loop ([a (for/fold ([a '()]) ([i n]) (cat-append (cat-of i) a))])
                   (unless (null? a) (loop (cat-rest a)))))
    ;; n elements; then n times: one added at the end, one removed
    (check-steps (* 3 n)
                 (for/fold ([a (list->cat (build-list n values))]) ([i n])
                   (cat-rest (cat-append a (cat-of i)))))))

(test-case "a list used again may cost more, and is still the same list"
  ;; the first `cat-rest` of this list reverses a queue of n elements,
  ;; and nothing remembers that it did
  (define n 1000)
  (define a (list->cat (build-list n values)))
  (define once (steps-of (cat-rest a)))
  (define again (steps-of (for ([_ 10]) (cat-rest a))))
  (check-true (>= again (* 10 (sub1 n))))
  (check-equal? (cat->list (cat-rest a)) (build-list (sub1 n) add1)))
