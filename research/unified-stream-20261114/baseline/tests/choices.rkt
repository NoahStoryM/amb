#lang racket/base

;; The forms of a choice (amb, amb*, amb-from, for/amb, for*/amb), the
;; shuffler, and the strategies that see all the alternatives (nary.rkt).

(require rackunit
         racket/list
         "../kernel.rkt"
         "../nary.rkt"
         (only-in "../model.rkt" reference-merge))

(define interleave-merge (reference-merge (quote interleave)))

(define-syntax-rule (events e) (amb-events (amb-reify (λ () e))))
(define-syntax-rule (events/fuel n e) (amb-events (amb-reify (λ () e)) n))
(define-syntax-rule (with m e ...) (parameterize ([current-amb-merge m]) e ...))

(define merges (list amb-append amb-interleave amb-alternate amb-level))

;; i, after i pauses
(define (later i) (for ([_ i]) (amb-pause)) i)
;; 0, 1, 2, ... with a pause before each recursive call
(define (count n) (amb n (begin (amb-pause) (count (add1 n)))))


;; ---- the forms ----

(test-case "a choice among many is the choice between the first and the others"
  (for ([m merges])
    (with m
      (check-equal? (events (amb (later 2) (later 1) (later 0) (later 1)))
                    (events (amb (later 2) (amb (later 1) (amb (later 0) (later 1)))))))))

(test-case "the alternatives in a vector, and from a sequence"
  ;; the same events for the four merges, because s ∨ '() = s
  (for ([m merges])
    (with m
      (define expected (events (amb (later 2) (later 1) (later 0) (later 1))))
      (check-equal? (events (later (amb-from '(2 1 0 1)))) expected)
      (check-equal? (events (for/amb ([i '(2 1 0 1)]) (later i))) expected)
      (check-equal? (events (for/amb #:length 4 ([i '(2 1 0 1)]) (later i))) expected)
      ;; fewer iterations than the length
      (check-equal? (events (for/amb #:length 9 ([i '(2 1 0 1)]) (later i))) expected)
      (check-equal? (events (for/amb #:length 9 ([i '()]) (later i))) '())
      (check-equal? (events (for/amb ([i '()]) (later i))) '())))
  (check-equal? (events (for*/amb ([i 3] [j i]) (list i j))) '((1 0) (2 0) (2 1)))
  (check-equal? (events (for*/amb #:length 3 ([i 3] [j i]) (list i j))) '((1 0) (2 0) (2 1)))
  (check-equal? (events (for/amb ([i 5]) #:break (= i 3) i)) '(0 1 2))
  (check-equal? (events (for/amb #:length 5 ([i 5]) #:break (= i 3) i)) '(0 1 2))
  ;; a choice in the body is a choice of the same search
  (check-equal? (events (for/amb ([i 2]) (list i (amb 'a 'b)))) '((0 a) (0 b) (1 a) (1 b))))

(test-case "a sequence is not asked for an element before its turn"
  ;; 0, and then the sequence fails to say whether there is more
  (define (seq)
    (make-do-sequence
     (λ () (values values add1 0 (λ (i) (or (zero? i) (error "asked for more"))) #f #f))))
  (check-equal? (events/fuel 1 (amb-from (seq))) '(0))
  (check-equal? (events/fuel 1 (for/amb ([i (seq)]) i)) '(0))
  (check-exn #rx"asked for more" (λ () (events/fuel 2 (amb-from (seq)))))
  ;; infinitely many alternatives
  (check-equal? (events/fuel 3 (for/amb ([i (in-naturals)]) (* i i))) '(0 1 4))
  ;; nor is an alternative of a vector started before its turn
  (check-equal? (events/fuel 1 (amb 0 (error "started"))) '(0)))

(test-case "a choice lets go of the alternatives it is done with"
  (define big (make-weak-box (make-vector 1000 0)))
  (define s
    (amb-reify (λ () (let ([v (weak-box-value big)])
                       (amb (begin (amb-pause) (vector-length v)) 2 3)))))
  ;; the first alternative has paused; it still needs the vector
  (collect-garbage)
  (check-true (vector? (weak-box-value big)))
  (define s1 (s))                       ; 1000, then the others
  (check-equal? (caar s1) 1000)
  (collect-garbage)
  (check-false (weak-box-value big))
  (check-equal? (amb-events s1) '(1000 2 3)))

(test-case "an alternative is taken once"
  (define s (amb-reify (λ () (amb 1 2 3))))
  (check-equal? (amb-events ((cdr s))) '(2 3))
  (check-exn #rx"single-pass" (λ () (amb-events ((cdr s))))))


;; ---- the shuffler ----

(define (reverse! v)
  (define n (vector-length v))
  (for ([i (quotient n 2)])
    (define x (vector-ref v i))
    (vector-set! v i (vector-ref v (- n i 1)))
    (vector-set! v (- n i 1) x)))

;; a uniform permutation
(define (shuffle! v)
  (for ([i (in-range (sub1 (vector-length v)) 0 -1)])
    (define j (random (add1 i)))
    (define x (vector-ref v i))
    (vector-set! v i (vector-ref v j))
    (vector-set! v j x)))

(test-case "the shuffler is called once, on all the alternatives of a choice"
  (define sizes '())
  (define (spy v) (set! sizes (cons (vector-length v) sizes)) (reverse! v))
  (parameterize ([current-amb-shuffler spy])
    (for ([m (append merges (list amb-rotate))])
      (with m
        (check-equal? (events (amb 1 2 3 4)) '(4 3 2 1))
        (check-equal? (events (for/amb #:length 9 ([i 4]) i)) '(3 2 1 0)))))
  (check-equal? (remove-duplicates sizes) '(4))
  ;; not for the alternatives of a sequence, which may have no end; and
  ;; not for the choice among the answers of a local search
  (set! sizes '())
  (parameterize ([current-amb-shuffler spy])
    (check-equal? (events (amb-from '(1 2 3))) '(1 2 3))
    (check-equal? (events (for/amb ([i 3]) i)) '(0 1 2))
    (check-equal? (events (amb-local (amb-from '(1 2 3)))) '(1 2 3))
    (check-equal? sizes '())))

(test-case "the shuffler is read where the choice is made"
  ;; as the strategy is: inside the search, it belongs to the computation
  (check-equal? (events (parameterize ([current-amb-shuffler reverse!])
                          (list (amb 1 2) (amb 'a 'b))))
                '((2 b) (2 a) (1 b) (1 a)))
  (check-equal? (events (list (parameterize ([current-amb-shuffler reverse!]) (amb 1 2))
                              (amb 'a 'b)))
                '((2 a) (2 b) (1 a) (1 b))))

(test-case "one permutation of a choice is not a permutation at every merge"
  ;; Astra's example (#22): how often c is the first answer
  (random-seed 20261113)
  (define trials 6000)
  (define-syntax-rule (share-of-c e)
    (/ (for/sum ([_ trials])
         (if (eq? 'c (car (events/fuel 1 e))) 1 0))
       trials))
  (define (near? x p) (< (abs (- x p)) 0.03))
  (parameterize ([current-amb-shuffler shuffle!])
    (check-true (near? (share-of-c (amb 'a 'b 'c)) 1/3))
    (check-true (near? (share-of-c (amb (amb 'a 'b) 'c)) 1/2))
    (check-true (near? (share-of-c (amb 'a (amb 'b 'c))) 1/4))))


(test-case "a permutation keeps the answers of a finite search"
  ;; whatever the strategy, when the search is run to its end
  (random-seed 20261113)
  (define-syntax-rule (answers e)
    (sort (filter pair? (events e)) string<? #:key (λ (x) (format "~a" x))))
  (define-syntax-rule (program)
    (let* ([x (amb 1 (later 2) 3)]
           [y (for/amb #:length 5 ([i x]) (later i))])
      (when (= x 1) (amb-pause))
      (list x y (amb 'a 'b))))
  (for ([m (append merges (list amb-rotate amb-dovetail))])
    (with m
      (define expected (answers (program)))
      (check-equal? (length expected) 12)
      (parameterize ([current-amb-shuffler shuffle!])
        (for ([_ 50])
          (check-equal? (answers (program)) expected))))))


;; ---- strategies that see all the alternatives ----

(define (str . es)                      ; a stream from its events
  (cond [(null? es) '()]
        [(eq? (car es) 'pause) (λ () (apply str (cdr es)))]
        [else (cons (list (car es)) (λ () (apply str (cdr es))))]))

;; the stream of the strategy for the streams of these lists of events
(define (merged m . ess)
  (amb-events ((amb-strategy-proc m)
               (λ () (and (pair? ess)
                          (begin0 (apply str (car ess)) (set! ess (cdr ess))))))))

(define (random-events)
  (for/list ([_ (random 6)])
    (if (zero? (random 2)) 'pause (random 100))))

(test-case "no alternatives, and one"
  (for ([m (list amb-rotate amb-dovetail amb-dovetail/unguarded)])
    (check-equal? (merged m) '()))
  (for ([_ 200])
    (define es (random-events))
    (check-equal? (merged amb-rotate es) es)
    (check-equal? (merged amb-dovetail/unguarded es) es))
  ;; amb-dovetail does not know that there is no second alternative
  ;; until it has asked, and it asks in the next stage
  (check-equal? (merged amb-dovetail '(a)) '(a pause))
  (check-equal? (merged amb-dovetail '(a pause b)) '(a pause b)))

(test-case "amb-rotate between two alternatives is amb-interleave"
  (for ([_ 2000])
    (define s (random-events))
    (define t (random-events))
    (check-equal? (merged amb-rotate s t)
                  (amb-events (interleave-merge (apply str s) (λ () (apply str t)))))
    (check-equal? (with amb-rotate (events (amb (amb-reflect (apply str s))
                                                (amb-reflect (apply str t)))))
                  (with amb-interleave (events (amb (amb-reflect (apply str s))
                                                    (amb-reflect (apply str t))))))))

(test-case "among three it is not: the alternatives are equals"
  (define (shares m)
    (define es
      (with m
        (filter pair? (events/fuel 2400 (amb (list 'a (with amb-append (count 0)))
                                             (list 'b (with amb-append (count 0)))
                                             (list 'c (with amb-append (count 0))))))))
    (for/list ([x '(a b c)])
      (/ (for/sum ([e es]) (if (eq? (car e) x) 1 0)) (length es))))
  (check-equal? (shares amb-interleave) '(1/2 1/4 1/4))
  (define (near? xs) (for/and ([x xs]) (< (abs (- x 1/3)) 1/100)))
  (check-true (near? (shares amb-rotate)))
  (check-true (near? (shares amb-dovetail)))
  ;; the turns, newest first
  (check-equal? (merged amb-rotate '(a pause a pause a) '(b pause b pause b) '(c pause c))
                '(a pause b pause a pause c pause b pause a c b)))

(test-case "the stages of amb-dovetail"
  (check-equal? (merged amb-dovetail '(a pause a pause a) '(b pause b pause b) '(c pause c))
                '(a pause a b pause a b c pause b c))
  (check-equal? (merged amb-dovetail/unguarded '(a pause a pause a) '(b pause b pause b) '(c pause c))
                '(a pause a b pause a b c pause b c))
  ;; every alternative is an answer at once
  (check-equal? (merged amb-dovetail '(a) '(b) '(c)) '(a pause b pause c pause))
  (check-equal? (merged amb-dovetail/unguarded '(a) '(b) '(c)) '(a b c))
  (check-equal? (merged amb-rotate '(a) '(b) '(c)) '(a b c))
  ;; every alternative fails
  (check-equal? (merged amb-dovetail '() '() '()) '(pause pause pause))
  (check-equal? (merged amb-dovetail/unguarded '() '() '()) '())
  ;; infinitely many
  (check-equal? (events/fuel 6 (amb-from (in-naturals) amb-dovetail))
                '(0 pause 1 pause 2 pause))
  (check-equal? (events/fuel 6 (amb-from (in-naturals) amb-dovetail/unguarded))
                '(0 1 2 3 4 5))
  (check-equal? (events/fuel 3 (begin (amb-from (in-naturals) amb-dovetail) (amb)))
                '(pause pause pause)))

;; Is x found, when it is the other alternative of a choice by `outer`
;; whose first alternative is a choice by `inner` among infinitely many?
;; The sequence gives up after 5000 elements.
(define (naturals/budget)
  (make-do-sequence
   (λ () (values values add1 0
                 (λ (i) (or (< i 5000) (error "starved")))
                 #f #f))))
(define (finds-x? outer inner fail?)
  (with-handlers ([exn:fail? (λ (e) #f)])
    (and (memq 'x (with outer
                    (events/fuel 4000
                                 (amb (let ([n (amb-from (naturals/budget) inner)])
                                        (when fail? (amb))
                                        n)
                                      'x))))
         #t)))

(test-case "a choice among infinitely many, and the alternative next to it"
  (define outers (list amb-append amb-interleave amb-alternate amb-level amb-rotate amb-dovetail))
  (define (row inner fail?) (for/list ([outer outers]) (finds-x? outer inner fail?)))
  ;; every one of the infinitely many is an answer at once
  (check-equal? (row amb-append #f)             '(#f #f #t #f #f #f))
  (check-equal? (row amb-rotate #f)             '(#f #f #t #f #f #f))
  (check-equal? (row amb-dovetail/unguarded #f) '(#f #f #t #f #f #f))
  (check-equal? (row amb-dovetail #f)           '(#f #t #t #t #t #t))
  ;; every one of them fails at once
  (check-equal? (row amb-append #t)             '(#f #f #f #f #f #f))
  (check-equal? (row amb-rotate #t)             '(#f #f #f #f #f #f))
  (check-equal? (row amb-dovetail/unguarded #t) '(#f #f #f #f #f #f))
  (check-equal? (row amb-dovetail #t)           '(#f #t #t #t #t #t)))
