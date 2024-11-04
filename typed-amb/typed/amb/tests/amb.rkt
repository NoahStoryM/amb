#lang typed/racket/base

(require typed/data/queue "../../amb.rkt")


(begin
  (with-handlers ([exn:fail:contract:amb? void])
    (parameterize ([current-amb-queue (make-queue)])
      (let ([x (amb 2 9 (amb))]
            [y (amb (amb) 9 2)])
        (ann x Real)
        (ann y : Real)
        (when (> x y) (amb))
        (displayln (list x y))
        (amb))))
  (newline))

(parameterize ([current-amb-queue (make-queue)])
  (let ([ls (amb '(a b c) '(x y z))])
    (displayln ls)
    (amb*)
    (newline)))

(parameterize ([current-amb-queue (make-queue)])
  (let ([b (amb #t (amb) (amb) #f)])
    (displayln (ann b Boolean))
    (newline)))

(parameterize ([current-amb-queue (make-queue)])
  (let ([x (amb (amb 0) (amb 1))])
    (displayln x)
    (newline)))

(parameterize ([current-amb-queue (make-queue)])
  (let-values ([(x y)
                (amb (values 9 2)
                     (values 2 9))])
    (when (> x y) (amb))
    (displayln (list x y))
    (newline)))

(parameterize ([current-amb-queue (make-queue)])
  (let-values ([(x y) (amb (values 2 9))])
    (when (> x y) (amb))
    (displayln (list x y))
    (newline)))

(parameterize ([current-amb-queue (make-queue)])
  (let-values ([(x y)
                (for/amb : (Values Real Real)
                         ([v1 : Real '(2 9)]
                          [v2 : Real '(9 2)])
                  (values v1 v2))])
    (when (> x y) (amb))
    (displayln (list x y))
    (newline)))

(parameterize ([current-amb-queue (make-queue)])
  (let-values ([(x y)
                (for/amb ([v1 : Real '(2 9)]
                          [v2 : Real '(9 2)])
                  : (Values Real Real)
                  (values v1 v2))])
    (when (> x y) (amb))
    (displayln (list x y))
    (newline)))

(parameterize ([current-amb-queue (make-queue)])
  (for ([i (in-amb (amb 1 2 (amb 3 'x) 'y))])
    (display i) (display #" "))
  (newline)
  (newline))

(parameterize ([current-amb-queue (make-queue)])
  (for ([(i) (in-amb/thunk (λ () (amb 1 2 (amb 3 'x) 'y)))])
    (display i) (display #" "))
  (newline)
  (newline))

(parameterize ([current-amb-queue (make-queue)])
  (for ([i : (∪ Symbol Number) (in-amb (amb 1 2 (amb 3 'x) 'y))])
    (display i) (display #" "))
  (newline)
  (newline))

(parameterize ([current-amb-queue (make-queue)])
  (for ([([i : (∪ Symbol Number)]) (in-amb/thunk (λ () (amb 1 2 (amb 3 'x) 'y)))])
    (display i) (display #" "))
  (newline)
  (newline))

(let ()
  (time
   (for ([i : Natural (in-range 100000)]
         [j : Natural (in-naturals)])
     (cons i j)))
  (newline))

(parameterize ([current-amb-queue (make-queue)])
  (: next (→ Integer Integer (Values Integer Integer)))
  (define (next i j) (amb (values i j) (next (add1 i) (sub1 j))))
  (time
   (for ([i : Natural (in-range 100000)]
         [([j : Integer] [k : Integer]) (in-amb/thunk (λ () (next 0 0)))])
     (list i j k)))
  (newline))

(parameterize ([current-amb-queue (make-queue)])
  (: next (→ Integer Integer (Values Integer Integer)))
  (define (next i j) (amb (values i j) (next (add1 i) (sub1 j))))
  (: s (Sequenceof Integer Integer))
  (define s (in-amb/thunk (λ () (next 0 0))))
  (time
   (for ([i : Natural (in-range 100000)]
         [([j : Integer] [k : Integer]) s])
     (list i j k)))
  (newline))

(parameterize ([current-amb-queue (make-queue)])
  (: next (→ Integer Integer))
  (define (next j) (amb j (next (add1 j))))
  (time
   (for ([i : Natural (in-range 100000)]
         [j : Integer (in-amb (next 0))])
     (list i j)))
  (newline))

(parameterize ([current-amb-queue (make-queue)])
  (: next (→ Integer Integer))
  (define (next j) (amb j (next (add1 j))))
  (: s (Sequenceof Integer))
  (define s (in-amb (next 0)))
  (time
   (for ([i : Natural (in-range 100000)]
         [j : Integer s])
     (list i j)))
  (newline))
