#lang racket/base

(require data/queue "../main.rkt")

(begin
  (with-handlers ([exn:fail:contract? void])
    (parameterize ([current-amb-queue (make-queue)])
      (let ([x (amb 2 1 -2 5  8 18)]
            [y (amb 9 8  2 4 14 20)])
        (when (> x y) (amb))
        (displayln (list x y))
        (amb))))
  (newline))

(begin
  (with-handlers ([exn:fail:contract? void])
    (parameterize ([current-amb-queue    (make-queue)]
                   [current-amb-enqueue! enqueue!])
      (let ([x (amb 1 2 3)])
        (displayln (list x))
        (let ([y (amb 4 5 6)])
          (displayln (list x y))
          (let ([z (amb 7 8 9)])
            (displayln (list x y z))
            (amb))))))
  (newline))

(parameterize ([current-amb-queue (make-queue)])
  (let-values ([(x y) (amb (values 2 9) (values 9 2))])
    (when (> x y) (amb))
    (displayln (list x y)))
  (newline))

(parameterize ([current-amb-queue (make-queue)])
  (let-values ([(x y)
                (for/amb ([v (in-list '([2 9] [9 2]))])
                  (apply values v))])
    (when (> x y) (amb))
    (displayln (list x y)))
  (newline))

(time
 (begin
   (with-handlers ([exn:fail:contract? void])
     (parameterize ([current-amb-queue (make-queue)])
       (let ([i (for/amb ([i (in-range 1 100000)]) i)])
         (amb))))
   (newline)))
