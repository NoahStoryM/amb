#lang racket/base

(require amb)

(with-handlers ([exn:fail:amb? void])
  (let ([x (amb 2 1 -2 5  8 18)]
        [y (amb 9 8  2 4 14 20)])
    (when (> x y) (amb))
    (displayln (list x y))
    (amb)))

(time
 (with-handlers ([exn:fail:amb? void])
   (let ([i (for/amb ([i (in-range 1 100000)]) i)])
     (when (even? i) (amb))
     (displayln i)
     (amb))))
