#lang typed/racket

(require "../main.rkt")

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
  (with-handlers ([exn:fail:amb? void])
    (parameterize ([current-amb-tree raise-amb-error])
      (let loop ([x x] [y y] [path path] [dir* dir*])
        (define pos (cons x y))
        (cond
          [(or (member pos path) (not (valid? x y))) (amb)]
          [(eq? '** (list-ref (list-ref maze x) y))
           (: res (Pair (Listof (Pair Integer Integer))
                        (Listof (U 'up 'down 'left 'right))))
           (define res (cons (reverse path) (reverse dir*)))
           (set! ans (cons res ans))
           (amb)]
          [else
           (let ([dir (amb 'up 'down 'left 'right)])
             (case dir
               [(up)    (loop (- x 1) y (cons pos path) (cons dir dir*))]
               [(down)  (loop (+ x 1) y (cons pos path) (cons dir dir*))]
               [(left)  (loop x (- y 1) (cons pos path) (cons dir dir*))]
               [(right) (loop x (+ y 1) (cons pos path) (cons dir dir*))]
               [else (amb)]))]))))
  ans)

(pretty-print (solve-maze 0 0 '() '()))
