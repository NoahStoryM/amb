#lang typed/racket/base

(require "../../data/queue.rkt" "../main.rkt")


(with-handlers ([exn:fail:contract? void])
  (parameterize ([current-amb-queue (make-queue)])
    (let ([x (amb 2 9 (ann (amb) : Real))]
          [y (amb (ann (amb) Real) 9 2)])
      (ann x Real)
      (ann y : Real)
      (when (> x y) (amb))
      (displayln (list x y))
      (amb))))

(with-handlers ([exn:fail:contract? void])
  (parameterize ([current-amb-queue (make-queue)])
    (let ([ls (amb '(a b c) '(x y z))])
      (displayln ls)
      (amb))))

(parameterize ([current-amb-queue (make-queue)])
  (let ([b (amb (ann (amb) True)
                (ann (amb) False)
                #t #f)])
    (displayln (ann b Boolean))))

(parameterize ([current-amb-queue (make-queue)])
  (let ([x : (U Zero One) (amb (ann 0 Zero) (ann 1 One))])
    (displayln x)))

(parameterize ([current-amb-queue (make-queue)])
  (let-values ([(x y)
                (for/amb : (Values Real Real)
                         ([v1 : Real (in-list '(2 9))]
                          [v2 : Real (in-list '(9 2))])
                  (values v1 v2))])
    (when (> x y) (amb))
    (displayln (list x y)))
  (newline))