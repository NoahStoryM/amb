#lang typed/racket/base

(require "../../data/queue.rkt" "../main.rkt")


(with-handlers ([exn:fail:contract? void])
  (parameterize ([current-amb-queue (make-queue)])
    (let ([x (amb : Real 2 9 (amb))]
          [y (amb : Real (amb) 9 2)])
      (ann x Real)
      (ann y : Real)
      (when (> x y) (amb))
      (displayln (list x y))
      (amb))))

(with-handlers ([exn:fail:contract? void])
  (parameterize ([current-amb-queue (make-queue)])
    (let ([ls (amb : (U '(a b c) '(x y z)) '(a b c) '(x y z))])
      (displayln ls)
      (amb))))

(parameterize ([current-amb-queue (make-queue)])
  (let ([b (amb : Boolean #t (amb : True) (amb : False) #f)])
    (displayln (ann b Boolean))))

(parameterize ([current-amb-queue (make-queue)])
  (let ([x : (U Zero One) (amb : Nothing (amb : Zero 0) (amb : One 1))])
    (displayln x)))

(parameterize ([current-amb-queue (make-queue)])
  (let-values ([(x y)
                (amb : (Values Real Real)
                     (values 9 2)
                     (values 2 9))])
    (when (> x y) (amb))
    (displayln (list x y)))
  (newline))

(parameterize ([current-amb-queue (make-queue)])
  (let-values ([(x y) (amb : (Values 2 9) (values 2 9))])
    (when (> x y) (amb))
    (displayln (list x y)))
  (newline))

(parameterize ([current-amb-queue (make-queue)])
  (let-values ([(x y)
                (for/amb : (Values Real Real)
                         ([v1 : Real (in-list '(2 9))]
                          [v2 : Real (in-list '(9 2))])
                  (values v1 v2))])
    (when (> x y) (amb))
    (displayln (list x y)))
  (newline))

(parameterize ([current-amb-queue (make-queue)])
  (let-values ([(x y)
                (for/amb ([v1 : Real (in-list '(2 9))]
                          [v2 : Real (in-list '(9 2))])
                  : (Values Real Real)
                  (values v1 v2))])
    (when (> x y) (amb))
    (displayln (list x y)))
  (newline))
