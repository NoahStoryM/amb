#lang racket/base

(require data/queue "../main.rkt")


(parameterize ([current-amb-queue (make-queue)])
  (let ([x (amb 2 1 -2 5  8 18)]
        [y (amb 9 8  2 4 14 20)])
    (when (> x y) (amb))
    (displayln (list x y))
    (amb* (newline))))

(parameterize ([current-amb-queue    (make-queue)]
               [current-amb-enqueue! enqueue!])
  (let ([x (amb 1 2 3)])
    (displayln (list x))
    (let ([y (amb 4 5 6)])
      (displayln (list x y))
      (let ([z (amb 7 8 9)])
        (displayln (list x y z))
        (amb* (newline))))))

(parameterize ([current-amb-queue (make-queue)])
  (let-values ([(x y) (amb (values 2 9) (values 9 2))])
    (when (> x y) (amb))
    (displayln (list x y))
    (newline)))

(parameterize ([current-amb-queue (make-queue)])
  (let-values ([(x y)
                (for/amb ([v '([2 9] [9 2])])
                  (apply values v))])
    (when (> x y) (amb))
    (displayln (list x y))
    (newline)))

(parameterize ([current-amb-queue (make-queue)])
  (let ([a (amb 'x 'y (let ([b (amb 1 2 3)]) (- b)) 'z)])
    (when (symbol? a) (amb))
    (displayln a)
    (newline)))

(parameterize ([current-amb-queue (make-queue)])
  (time
   (let ([i (for/amb ([i 100000]) i)])
     (amb*)
     (displayln i)))
  (newline))

(parameterize ([current-amb-queue (make-queue)])
  (time
   (define m 100000)
   (define n (for/amb ([i (in-inclusive-range 0 m)]) i))
   (when (< n m) (amb))
   (displayln n))
  (newline))

(parameterize ([current-amb-queue (make-queue)])
  (time
   (define n (let next ([i 0]) (amb i (next (add1 i)))))
   (when (< n 100000) (amb))
   (displayln n))
  (newline))

(let ()
  (time
   (for ([i (in-range 100000)]
         [j (in-naturals)])
     (cons i j)))
  (newline))

(parameterize ([current-amb-queue (make-queue)])
  (define (next j) (amb j (next (add1 j))))
  (time
   (for ([i (in-range 100000)]
         [j (in-amb (next 0))])
     (list i j)))
  (newline))

(parameterize ([current-amb-queue (make-queue)])
  (define (next i j) (amb (values i j) (next (add1 i) (sub1 j))))
  (time
   (for ([i (in-range 100000)]
         [(j k) (in-amb (next 0 0))])
     (list i j k)))
  (newline))
