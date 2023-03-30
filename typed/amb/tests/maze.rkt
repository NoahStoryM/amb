#lang typed/racket

(require "../../data/queue.rkt" "../main.rkt")

(current-amb-shuffler shuffle)

(: maze (Listof (Listof (U #t #f '**))))
(define maze
  '((#t #t #t #t #t #t #t #t #t #t)
    (#t #f #f #f #f #f #f #f #f #t)
    (#t #f #t #t #t #t #t #t #f #t)
    (#t #t #t #f #f #f #f #f #f #t)
    (#t #f #t #t #t #t #t #t #t #t)
    (#t #f #f #f #f #f ** #f #f #f)))

(: valid? (-> Integer Integer Boolean))
(define (valid? x y)
  (and (index? x) (< x (length maze))
       (index? y) (< y (length (car maze)))
       (list-ref (assert (list-ref maze x)) y)
       #t))

(: solve-maze (-> Integer Integer
                  (Listof (Pair Integer Integer))
                  (Listof (U 'up 'down 'left 'right))
                  (Listof (Pair (Listof (Pair Integer Integer))
                                (Listof (U 'up 'down 'left 'right))))))
(define (solve-maze x y path dir*)
  (: ans (Listof (Pair (Listof (Pair Integer Integer))
                       (Listof (U 'up 'down 'left 'right)))))
  (define ans '())
  (with-handlers ([exn:fail:contract? void])
    (parameterize ([current-amb-queue (make-queue)])
      (let loop ([x x] [y y] [path path] [dir* dir*])
        (unless (valid? x y) (amb))
        (define pos (cons x y))
        (when (member pos path) (amb))
        (cond
          [(eq? '** (list-ref (list-ref maze x) y))
           (: res (Pair (Listof (Pair Integer Integer))
                        (Listof (U 'up 'down 'left 'right))))
           (define res (cons (reverse path) (reverse dir*)))
           (set! ans (cons res ans))
           (amb)]
          [else
           (let-values ([(dir x y)
                         (amb
                          (ann (amb) (Values (U 'up 'down 'left 'right) Integer Integer))
                          (values 'up    (sub1 x) y)
                          (values 'down  (add1 x) y)
                          (values 'left  x (sub1 y))
                          (values 'right x (add1 y)))])
             (loop x y (cons pos path) (cons dir dir*)))]))))
  ans)

(pretty-print (solve-maze 0 0 '() '()))
