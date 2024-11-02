#lang typed/racket/base

(require typed/data/queue "../main.rkt")


(begin
  (with-handlers ([exn:fail:contract:amb? void])
    (parameterize ([current-amb-queue (make-queue)])
      (let ([x (amb : Real 2 9 (amb))]
            [y (amb : Real (amb) 9 2)])
        (ann x Real)
        (ann y : Real)
        (when (> x y) (amb))
        (displayln (list x y))
        (amb))))
  (newline))

(parameterize ([current-amb-queue (make-queue)])
  (let ([ls (amb : (∪ '(a b c) '(x y z)) '(a b c) '(x y z))])
    (displayln ls)
    (amb*)
    (newline)))

(parameterize ([current-amb-queue (make-queue)])
  (let ([b (amb : Boolean #t (amb : True) (amb : False) #f)])
    (displayln (ann b Boolean))
    (newline)))

(parameterize ([current-amb-queue (make-queue)])
  (let ([x (amb : (∪ Zero One) (amb : Zero 0) (amb : One 1))])
    (displayln x)
    (newline)))

(parameterize ([current-amb-queue (make-queue)])
  (let-values ([(x y)
                (amb : (Values Real Real)
                     (values 9 2)
                     (values 2 9))])
    (when (> x y) (amb))
    (displayln (list x y))
    (newline)))

(parameterize ([current-amb-queue (make-queue)])
  (let-values ([(x y) (amb : (Values 2 9) (values 2 9))])
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
  (for ([i (in-amb (amb : Any 1 2 (amb : Any 3 'x) 'y))])
    (displayln i))
  (newline))