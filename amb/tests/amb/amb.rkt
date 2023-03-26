#lang racket/base

(require amb)

(begin
  (with-handlers ([exn:fail:amb? void])
    (parameterize ([current-amb-tree raise-amb-error])
      (let ([x (amb 2 1 -2 5  8 18)]
            [y (amb 9 8  2 4 14 20)])
        (when (> x y) (amb))
        (displayln (list x y))
        (amb))))
  (newline))



(parameterize ([current-amb-tree raise-amb-error])
  (let-values ([(x y) (amb (values 2 9) (values 9 2))])
    (when (> x y) (amb))
    (displayln (list x y)))
  (newline))

(parameterize ([current-amb-tree raise-amb-error])
  (let-values ([(x y)
                (for/amb ([v (in-list '([2 9] [9 2]))])
                  (apply values v))])
    (when (> x y) (amb))
    (displayln (list x y)))
  (newline))


(time
 (begin
   (with-handlers ([exn:fail:amb? void])
     (parameterize ([current-amb-tree raise-amb-error])
       (let ([i (for/amb ([i (in-range 1 100000)]) i)])
         (amb))))
   (newline)))
