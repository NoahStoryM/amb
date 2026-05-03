#lang typed/racket/base

(require racket/sequence typed/rackunit)
(require "../../amb.rkt")

(displayln "Test `amb.rkt'")

(: sequence-first (∀ (a ...) (→ (Sequenceof a ... a) (Values a ... a))))
(define (sequence-first s) (sequence-ref s 0))

(test-case "Test amb operator"
  (let ([ls (sequence-first (in-amb (amb '(a b c) '(x y z))))])
    (check-equal? ls '(a b c)))
  (let ([b (sequence-first (in-amb (amb #t (amb) (amb) #f)))])
    (check-true (ann b Boolean)))
  (let ([x (sequence-first (in-amb (amb (amb 0) (amb 1))))])
    (check-eq? x 0))
  (let ()
    (define (thk)
      (let-values ([(x y) (amb (values 9 2) (values 2 9))])
        (when (> x y) (amb))
        (list x y)))
    (check-equal? (sequence-first (in-amb* thk)) '(2 9)))
  (let ()
    (define (thk)
      (let-values ([(x y) (amb (values 2 9))])
      (when (> x y) (amb))
      (list x y)))
    (check-equal? (sequence-first (in-amb* thk)) '(2 9))))

(test-case "Test amb empty handler"
  (define (thk)
    (: res (Listof (List Real Real)))
    (define res '())
    (sequence-length
     (in-amb
      (let ([x (amb 2 9 (amb))]
            [y (amb (amb) 9 2)])
        (ann x Real)
        (ann y : Real)
        (when (> x y) (amb))
        (set! res (cons (list x y) res))
        (amb))))
    res)
  (check-equal? (thk) '((9 9) (2 2) (2 9))))

(test-case "Test for/amb"
  (let ()
    (define (thk)
      (let-values ([(x y)
                    (for/amb : (Values Real Real)
                             ([v1 : Real '(2 9)]
                              [v2 : Real '(9 2)])
                      (values v1 v2))])
        (when (> x y) (amb))
        (list x y)))
    (check-equal? (sequence-first (in-amb* thk)) '(2 9)))
  (let ()
    (define (thk)
      (let-values ([(x y)
                    (for/amb ([v1 : Real '(2 9)]
                              [v2 : Real '(9 2)])
                             : (Values Real Real)
                      (values v1 v2))])
        (when (> x y) (amb))
        (list x y)))
    (check-equal? (sequence-first (in-amb* thk)) '(2 9)))
  (check-equal?
   (for/list ([(a b) (in-amb (for/amb #:length 4
                                      ([i #(1 2)] [j #(x y)])
                                      : (Values Integer Symbol)
                               (values i j)))])
             : (Listof (Pair Integer Symbol))
     (cons a b))
   '((1 . x) (2 . y)))
  (check-equal?
   (for/list ([(a b) (in-amb (for/amb #:length 4 #:fill (values 0 'w)
                                      ([i #(1 2)] [j #(x y)])
                                      : (Values Integer Symbol)
                               (values i j)))])
             : (Listof (Pair Integer Symbol))
     (cons a b))
   '((1 . x) (2 . y) (0 . w) (0 . w))))

(test-case "Test in-amb[*]"
  (check-equal?
   (for/list : (Listof Any) ([i (in-amb (amb 1 2 (amb 3 'x) 'y))]) i)
   '(1 2 3 x y))
  (check-equal?
   (for/list ([(i) (in-amb* (λ () (amb 1 2 (amb 3 'x) 'y)))])
             : (Listof Any)
     i)
   '(1 2 3 x y))
  (check-equal?
   (for/list : (Listof Any)
             ([i : (∪ Symbol Integer) (in-amb (amb 1 2 (amb 3 'x) 'y))])
     i)
   '(1 2 3 x y))
  (let ()
    (define (thk) (amb 1 2 (amb 3 'x) 'y))
    (check-equal?
     (for/list ([([i : (∪ Symbol Integer)]) (in-amb* thk)])
               : (Listof Any)
       i)
     '(1 2 3 x y))))

(test-case "Test parameters"
  (check-equal?
   (for/list : (Listof Any)
             ([x : Integer (in-amb (amb 1 (amb 2 3) (amb 4 5 6) 7 8))])
     x)
   '(1 2 3 4 5 6 7 8))
  (check-equal?
   (parameterize ([current-amb-depth-first? #f])
     (for/list : (Listof Any)
               ([x : Integer (in-amb (amb 1 (amb 2 3) (amb 4 5 6) 7 8))])
       x))
   '(1 7 8 2 3 4 5 6))
  (check-equal?
   (for/list : (Listof Any)
             ([i : (∪ Symbol Integer) (in-amb (amb (amb 1 2 3) (amb 'a 'b 'c)))])
     i)
   '(1 2 3 a b c))
  (check-equal?
   (parameterize ([current-amb-fair? #t])
     (for/list : (Listof Any)
               ([i : (∪ Symbol Integer) (in-amb (amb (amb 1 2 3) (amb 'a 'b 'c)))])
       i))
   '(1 a 2 b 3 c)))

(test-case "Test #:break / #:final"
  (test-case "#:break (stream path)"
    (check-equal?
     (for/list : (Listof Integer)
               ([x (in-amb (for/amb ([i (in-range 10)]) : Integer
                                    #:break (>= i 3) i))])
       x)
     '(0 1 2)))

  (test-case "#:final (stream path)"
    (check-equal?
     (for/list : (Listof Integer)
               ([x (in-amb (for/amb ([i (in-range 10)]) : Integer
                                    #:final (>= i 3) i))])
       x)
     '(0 1 2 3)))

  (test-case "#:break with type-first form"
    (check-equal?
     (for/list : (Listof Integer)
               ([x (in-amb (for/amb : Integer ([i (in-range 10)])
                             #:break (>= i 5) i))])
       x)
     '(0 1 2 3 4))))

(test-case "Test efficiency"
  (time
   (define m 10000000)
   (define s (in-amb (for/amb #:length m ([i (in-range m)]) : Integer i)))
   (void))
  (time
   (define m 10000000)
   (define s (in-amb (for/amb ([i (in-range m)]) : Integer i)))
   (void))
  (time
   (: next (→ Integer Integer))
   (define (next j) (amb j (next (add1 j))))
   (: s (Sequenceof Integer))
   (define s (in-amb (next 0)))
   (for ([i : Index 1000000]
         [j : Integer s])
     (list i j)))
  (time
   (: next (→ Integer Integer))
   (define (next j) (amb j (next (add1 j))))
   (: s (Sequenceof Integer))
   (define s (in-amb* (λ () (for/amb #:length 1000000 ([i 1000000]) : Integer i))))
   (for ([i : Index 1000000]
         [j : Integer s])
     (list i j)))
  (time
   (: next (→ Integer Integer))
   (define (next j) (amb j (next (add1 j))))
   (: s (Sequenceof Integer))
   (define s (in-amb* (λ () (for/amb ([i 1000000]) : Integer i))))
   (for ([i : Index 1000000]
         [j : Integer s])
     (list i j)))
  (time
   (: next (→ Integer Integer (Values Integer Integer)))
   (define (next i j) (amb (values i j) (next (add1 i) (sub1 j))))
   (define s (in-amb* (λ () (next 0 0))))
   (for ([i : Index 1000000]
         [([j : Integer] k) s])
     (list i j k)))
  (time
   (: next (→ Integer Integer (Values Integer Integer)))
   (define (next i j) (amb (values i j) (next (add1 i) (sub1 j))))
   (for ([i : Index 1000000]
         [([j : Integer] k) (in-amb* (λ () (next 0 0)))])
     (list i j k)))
  (time
   (: next (→ Integer Integer (Values Integer Integer)))
   (define (next i j) (amb (values i j) (next (add1 i) (sub1 j))))
   (define s (in-amb (next 0 0)))
   (for ([i : Index 1000000]
         [([j : Integer] k) s])
     (list i j k)))
  (time
   (: next (→ Integer Integer (Values Integer Integer)))
   (define (next i j) (amb (values i j) (next (add1 i) (sub1 j))))
   (for ([i : Index 1000000]
         [([j : Integer] k) (in-amb (next 0 0))])
     (list i j k)))
  (parameterize ([current-amb-shuffler (λ (v) (void))])
    (time (for ([i (in-amb (for/amb #:length 1000000 ([i 1000000]) : Index i))]) i)))
  (parameterize ([current-amb-shuffler (λ (v) (void))])
    (time (for ([i (in-amb (for/amb ([i 1000000]) : Index i))]) i)))
  (time (for ([i (in-amb (for/amb : Index #:length 1000000 ([i 1000000]) i))]) i))
  (time (for ([i (in-amb (for/amb : Index ([i 1000000]) i))]) i)))
