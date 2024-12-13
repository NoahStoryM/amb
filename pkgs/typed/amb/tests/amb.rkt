#lang typed/racket/base

(require typed/rackunit)
(require "../../amb.rkt")

(displayln "Test `amb.rkt'")

(test-case "Test amb operator"
  (parameterize ([current-amb-tasks ((current-amb-maker))])
    (let ([ls (amb '(a b c) '(x y z))])
      (check-equal? ls '(a b c))))
  (parameterize ([current-amb-tasks ((current-amb-maker))])
    (let ([b (amb #t (amb) (amb) #f)])
      (check-true (ann b Boolean))))
  (parameterize ([current-amb-tasks ((current-amb-maker))])
    (let ([x (amb (amb 0) (amb 1))])
      (check-eq? x 0)))
  (parameterize ([current-amb-tasks ((current-amb-maker))])
    (let-values ([(x y) (amb (values 9 2) (values 2 9))])
      (when (> x y) (amb))
      (check-equal? (list x y) '(2 9))))
  (parameterize ([current-amb-tasks ((current-amb-maker))])
    (let-values ([(x y) (amb (values 2 9))])
      (when (> x y) (amb))
      (check-equal? (list x y) '(2 9)))))

(test-case "Test amb empty handler"
  (: res (Listof (List Real Real)))
  (define res '())
  (let/cc break : (Values)
    (parameterize ([current-amb-tasks ((current-amb-maker))]
                   [current-amb-empty-handler break])
      (let ([x (amb 2 9 (amb))]
            [y (amb (amb) 9 2)])
        (ann x Real)
        (ann y : Real)
        (when (> x y) (amb))
        (set! res (cons (list x y) res))
        (amb))))
  (check-equal? res '((9 9) (2 2) (2 9))))

(test-case "Test for/amb"
  (parameterize ([current-amb-tasks ((current-amb-maker))])
    (let-values ([(x y)
                  (for/amb : (Values Real Real)
                           ([v1 : Real '(2 9)]
                            [v2 : Real '(9 2)])
                    (values v1 v2))])
      (when (> x y) (amb))
      (check-equal? (list x y) '(2 9))))
  (parameterize ([current-amb-tasks ((current-amb-maker))])
    (let-values ([(x y)
                  (for/amb ([v1 : Real '(2 9)]
                            [v2 : Real '(9 2)])
                           : (Values Real Real)
                    (values v1 v2))])
      (when (> x y) (amb))
      (check-equal? (list x y) '(2 9)))))

(test-case "Test in-amb[*]"
  (parameterize ([current-amb-tasks ((current-amb-maker))])
    (check-equal?
     (for/list : (Listof Any) ([i (in-amb (amb 1 2 (amb 3 'x) 'y))]) i)
     '(1 2 3 x y)))
  (parameterize ([current-amb-tasks ((current-amb-maker))])
    (check-equal?
     (for/list ([(i) (in-amb* (λ () (amb 1 2 (amb 3 'x) 'y)))]) : (Listof Any) i)
     '(1 2 3 x y)))
  (parameterize ([current-amb-tasks ((current-amb-maker))])
    (check-equal?
     (for/list : (Listof Any) ([i : (∪ Symbol Number) (in-amb (amb 1 2 (amb 3 'x) 'y))]) i)
     '(1 2 3 x y)))
  (parameterize ([current-amb-tasks ((current-amb-maker))])
    (define (thk) (amb 1 2 (amb 3 'x) 'y))
    (check-equal?
     (for/list ([([i : (∪ Symbol Number)]) (in-amb* thk)]) : (Listof Any) i)
     '(1 2 3 x y))))

(test-case "Test efficiency"
  (parameterize ([current-amb-tasks ((current-amb-maker))])
    (time
     (define m 10000000)
     (define n (for/amb #:length m ([i (in-range m)]) : Integer i))
     (void)))
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
  (time (for ([i (in-amb (for/amb : Index #:length 1000000 ([i 1000000]) i))]) i)))
