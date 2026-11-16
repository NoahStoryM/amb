#lang racket/base

;; V3-PLAN section 9: the code that supplies the alternatives of a
;; choice must not choose, fail or pause in the search that it supplies.

(require rackunit
         racket/stream
         amb
         amb/merge)

(define-syntax-rule (answers e) (for/list ([x (in-amb/do e)]) x))
(define-syntax-rule (check-supply-error e)
  (check-exn #rx"not allowed while the alternatives of a choice are supplied"
             (λ () (answers e))))
(define mergers (list amb-append amb-interleave amb-alternate amb-level))

(test-case "a choice in a clause is an error, with and without #:length"
  ;; A-D of #22
  (check-supply-error (for/amb ([x '(1 2 3)] #:when (amb #t #f)) x))
  (check-supply-error (for/amb #:length 3 ([x '(1 2 3)] #:when (amb #t #f)) x))
  (check-supply-error (for/amb ([x '(1 2 3)]) #:break (and (= x 2) (amb #t #f)) x))
  (check-supply-error (for/amb #:length 3 ([x '(1 2 3)]) #:break (and (= x 2) (amb #t #f)) x))
  (check-supply-error (for*/amb ([x '(1 2)] [y (in-range (amb 1 2))]) (+ (* 10 x) y)))
  (check-supply-error (for*/amb #:length 4 ([x '(1 2)] [y (in-range (amb 1 2))]) (+ (* 10 x) y)))
  (check-supply-error (for/amb ([x (in-range (amb 1 2))]) x))
  (check-supply-error (for/amb #:length 2 ([x (in-range (amb 1 2))]) x)))

(test-case "so are failure and a pause, and an effect in a function that a clause calls"
  (define (choose-bound) (amb 1 2))
  (define (keep? x) (when (odd? x) (amb-fail)) #t)
  (check-supply-error (for/amb ([x (in-range (choose-bound))]) x))
  (check-supply-error (for/amb #:length 2 ([x (in-range (choose-bound))]) x))
  (check-supply-error (for/amb ([x '(1 2 3)] #:when (keep? x)) x))
  (check-supply-error (for/amb #:length 3 ([x '(1 2 3)] #:when (keep? x)) x))
  (check-supply-error (for/amb ([x '(1 2 3)] #:when (begin (amb-pause) #t)) x))
  (check-supply-error (for/amb #:length 3 ([x '(1 2 3)] #:when (begin (amb-pause) #t)) x))
  (check-exn #rx"^amb-fail: not allowed" (λ () (answers (for/amb ([x '(1 2 3)] #:when (keep? x)) x))))
  (check-exn #rx"^amb-pause: not allowed"
             (λ () (answers (for/amb ([x '(1)] #:when (begin (amb-pause) #t)) x))))
  (check-exn #rx"^make-amb: not allowed" (λ () (answers (for/amb ([x (in-range (amb 1 2))]) x)))))

(test-case "the length expression supplies alternatives, including at zero length"
  (check-supply-error (for/amb #:length (amb 1 2) ([x '(10 20)]) x))
  (check-supply-error (for*/amb #:length (amb 1 2) ([x '(10 20)]) x))
  (check-supply-error (for/amb #:length (begin (amb-pause) 0) ([x '(10 20)]) x))
  (check-supply-error (for/amb #:length (begin (amb-fail) 0) ([x '(10 20)]) x))
  (check-supply-error (for/amb #:length (amb 0 1) #:fill 99 ([x '(10 20)]) x))
  (check-supply-error (for*/amb #:length (begin (amb-pause) 0) #:fill 99 ([x '(10 20)]) x))
  ;; Pure length expressions run once; zero does not initialize clauses or fill.
  (define lengths 0)
  (check-equal? (answers (for/amb #:length (begin (set! lengths (add1 lengths)) 0)
                          #:fill (error "early fill")
                          ([x (error "early clause")]) x))
                '())
  (check-equal? lengths 1)
  ;; An independent search inside the length expression is allowed.
  (check-equal? (answers (for/amb #:length (length (answers (amb 1 2)))
                          ([x '(10 20)]) (amb x (- x))))
                '(10 -10 20 -20)))

(test-case "the generator of make-amb, the sequence of sequence->amb, the shuffler"
  (check-supply-error (make-amb (λ () (amb-pause) (values '(1) (λ () (values #f void))))))
  (check-supply-error (make-amb (λ () (values '(1) (λ () (amb-fail))))))
  (check-supply-error (sequence->amb (in-producer (λ () (amb 1 2)))))
  (check-supply-error (parameterize ([current-amb-shuffler (λ (v) (amb-pause) (void))])
                        (amb 1 2)))
  ;; a generator pauses by returning a number
  (check-equal? (for/list ([x (in-amb/do (make-amb (λ () (values 1 (λ () (values '(a) (λ () (values #f void)))))))
                                         #:on-pause (λ () 'P))])
                  x)
                '(P a)))

(test-case "the error is raised where the effect is reached, not before"
  ;; the second row is not asked for
  (check-equal? (for/first ([x (in-amb/do (for/amb ([x '(1 2 3)] #:when (or (= x 1) (amb #t #f))) x))]) x)
                1)
  ;; with #:length all the rows are collected first
  (check-exn #rx"not allowed"
             (λ () (for/first ([x (in-amb/do (for/amb #:length 3 ([x '(1 2 3)] #:when (or (= x 1) (amb #t #f))) x))])
                     x)))
  ;; a clause that is not reached
  (check-equal? (answers (for/amb ([x '(1 2 3)] #:when (or #t (amb #t #f))) x)) '(1 2 3))
  ;; after the error, the search can go on with its other alternatives
  (define-values (more? get)
    (sequence-generate (in-amb/do (amb 0 (for/amb ([x (in-range (amb 1 2))]) x) 9))))
  (check-equal? (get) 0)
  (check-exn #rx"not allowed" get))

(test-case "the body is part of the search"
  ;; 1 and 8 of tests/for-amb.rkt
  (for ([m mergers])
    (parameterize ([current-amb-merger m])
      (check-equal? (sort (answers (for/amb ([x '(1 2)]) (amb x (- x)))) <) '(-2 -1 1 2))
      (check-equal? (sort (answers (for/amb #:length 2 ([x '(1 2)]) (amb x (- x)))) <) '(-2 -1 1 2))
      (check-equal? (sort (answers (for*/amb ([x '(1 2)]) (+ (* 10 x) (amb 1 2)))) <) '(11 12 21 22))
      (check-equal? (sort (answers (for/amb ([x '(1 2 3)]) (when (odd? x) (amb-fail)) (amb-pause) x)) <)
                    '(2))
      (check-equal? (sort (map (λ (x) (format "~a" x)) (answers (for/amb #:length 3 #:fill (amb 'a 'b) ([x '(1)]) x))) string<?)
                    '("1" "a" "a" "b" "b")))))

(test-case "a search of its own in a clause is not concerned"
  (for ([m mergers])
    (parameterize ([current-amb-merger m])
      ;; run to its end in the clause
      (check-equal? (answers (for/amb ([x (stream->list (in-amb (amb 1 (begin (amb-pause) 2) (amb))))]) x))
                    '(1 2))
      (check-equal? (answers (for/amb #:length 2 ([x (stream->list (in-amb (amb 1 (begin (amb-pause) 2))))]) x))
                    '(1 2))
      ;; resumed by the supply, an element at a time
      (check-equal? (answers (for/amb ([x (in-amb/do (let ([y (amb 1 2)]) (amb-pause) (amb y (- y))))]) x))
                    (answers (let ([y (amb 1 2)]) (amb-pause) (amb y (- y)))))
      (check-equal? (sort (answers (sequence->amb (in-amb (for/amb ([x '(1 2)]) (amb x (- x)))))) <)
                    '(-2 -1 1 2))
      ;; a local search: the generator of make-amb goes on with a search
      (check-equal? (answers (make-amb (λ () (amb-generate* (λ () (amb 1 (begin (amb-pause) 2)))))))
                    '(1 2))
      ;; in a clause of a search in a clause
      (check-equal? (sort (answers (for/amb ([x (in-amb/do (for/amb ([y (in-amb/do (amb 1 2))]) (amb y (* 10 y))))]) x)) <)
                    '(1 2 10 20))))
  ;; but the search in the clause has clauses too
  (check-supply-error (for/amb ([x (in-amb/do (for/amb ([y (in-range (amb 1 2))]) y))]) x)))

(test-case "outside any search"
  (check-exn #rx"^make-amb: not in a search" (λ () (amb 1 2)))
  (check-exn #rx"^amb-fail: not in a search" (λ () (amb-fail)))
  (check-exn #rx"^amb-pause: not in a search" (λ () (amb-pause)))
  ;; supplied to no search: a stream of alternatives forced by hand
  (check-exn #rx"^make-amb: not in a search"
             (λ () (for/list ([x (in-producer (λ () (amb 1 2)))] [_ 1]) x))))

(test-case "with-amb-mark and call-with-amb-mark"
  ;; a form of one's own that collects its alternatives first
  (define (choose-among thunks) ((sequence->amb thunks)))
  (check-equal? (answers (choose-among (with-amb-mark (for/list ([i 3]) (λ () (amb i (- i)))))))
                '(0 0 1 -1 2 -2))
  (check-supply-error
   (choose-among (with-amb-mark (for/list ([i 3] #:when (amb #t #f)) (λ () i)))))
  (check-supply-error
   (choose-among (call-with-amb-mark (λ () (for/list ([i 3] #:when (amb #t #f)) (λ () i))))))
  ;; without the mark, the clause is part of the search: both branches
  ;; of the choice go on with the same loop
  (check-equal? (answers (choose-among (for/list ([i 2] #:when (amb #t #f)) (λ () i))))
                '(0 1 0 1))
  ;; a search of its own inside is not concerned
  (check-equal? (with-amb-mark (answers (amb 1 2))) '(1 2))
  (check-equal? (answers (with-amb-mark (answers (amb 1 2)))) '((1 2)))
  ;; the values of the expression
  (check-equal? (call-with-values (λ () (with-amb-mark (values 1 2))) list) '(1 2))
  (check-equal? (call-with-values (λ () (with-amb-mark (values))) list) '())
  ;; Leaving the helper also leaves its mark, including by exception.
  (check-exn #rx"boom" (λ () (with-amb-mark (error "boom"))))
  (check-equal? (answers (begin (with-amb-mark (void)) (amb 1 2))) '(1 2)))
