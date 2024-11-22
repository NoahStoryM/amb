#lang typed/racket/base

(require typed/data/queue typed/rackunit)
(require "../../amb.rkt")


(test-case "Test amb operator"
  (parameterize ([current-amb-queue (make-queue)])
    (let ([ls (amb '(a b c) '(x y z))])
      (check-equal? ls '(a b c))))
  (parameterize ([current-amb-queue (make-queue)])
    (let ([b (amb #t (amb) (amb) #f)])
      (check-true (ann b Boolean))))
  (parameterize ([current-amb-queue (make-queue)])
    (let ([x (amb (amb 0) (amb 1))])
      (check-eq? x 0)))
  (parameterize ([current-amb-queue (make-queue)])
    (let-values ([(x y) (amb (values 9 2) (values 2 9))])
      (when (> x y) (amb))
      (check-equal? (list x y) '(2 9))))
  (parameterize ([current-amb-queue (make-queue)])
    (let-values ([(x y) (amb (values 2 9))])
      (when (> x y) (amb))
      (check-equal? (list x y) '(2 9)))))

(test-case "Test amb empty handler"
  (: res (Listof (List Real Real)))
  (define res '())
  (let/cc break : (Values)
    (parameterize ([current-amb-queue (make-queue)]
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
  (parameterize ([current-amb-queue (make-queue)])
    (let-values ([(x y)
                  (for/amb : (Values Real Real)
                           ([v1 : Real '(2 9)]
                            [v2 : Real '(9 2)])
                    (values v1 v2))])
      (when (> x y) (amb))
      (check-equal? (list x y) '(2 9))))
  (parameterize ([current-amb-queue (make-queue)])
    (let-values ([(x y)
                  (for/amb ([v1 : Real '(2 9)]
                            [v2 : Real '(9 2)])
                           : (Values Real Real)
                    (values v1 v2))])
      (when (> x y) (amb))
      (check-equal? (list x y) '(2 9)))))

(test-case "Test in-amb[*]"
  (parameterize ([current-amb-queue (make-queue)])
    (check-equal?
     (for/list : (Listof Any) ([i (in-amb (amb 1 2 (amb 3 'x) 'y))]) i)
     '(1 2 3 x y)))
  (parameterize ([current-amb-queue (make-queue)])
    (check-equal?
     (for/list ([(i) (in-amb* (λ () (amb 1 2 (amb 3 'x) 'y)))]) : (Listof Any) i)
     '(1 2 3 x y)))
  (parameterize ([current-amb-queue (make-queue)])
    (check-equal?
     (for/list : (Listof Any) ([i : (∪ Symbol Number) (in-amb (amb 1 2 (amb 3 'x) 'y))]) i)
     '(1 2 3 x y)))
  (parameterize ([current-amb-queue (make-queue)])
    (define (thk) (amb 1 2 (amb 3 'x) 'y))
    (check-equal?
     (for/list ([([i : (∪ Symbol Number)]) (in-amb* thk)]) : (Listof Any) i)
     '(1 2 3 x y))))

(test-case "Test efficiency"
  (time
   (: next (→ Integer Integer (Values Integer Integer)))
   (define (next i j) (amb (values i j) (next (add1 i) (sub1 j))))
   (for ([i : Index 100000]
         [([j : Integer] [k : Integer]) (in-amb* (λ () (next 0 0)))])
     (list i j k)))
  (time
   (: next (→ Integer Integer))
   (define (next j) (amb j (next (add1 j))))
   (for ([i : Index 100000]
         [j : Integer (in-amb (next 0))])
     (list i j)))
  (time
   (: next (→ Integer Integer))
   (define (next j) (amb j (next (add1 j))))
   (: s (Sequenceof Integer))
   (define s (in-amb (next 0)))
   (for ([i : Index 100000]
         [j : Integer s])
     (list i j))))
