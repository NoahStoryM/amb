#lang racket

(require "../main.rkt")

(current-amb-shuffler shuffle)

(define maze
  '((#t #t #t #t #t #t #t #t #t #t)
    (#t #f #f #f #f #f #f #f #f #t)
    (#t #f #t #t #t #t #t #t #f #t)
    (#t #t #t #f #f #f #f #f #f #t)
    (#t #f #t #t #t #t #t #t #t #t)
    (#t #f #f #f #f #f ** #f #f #f)))

(define (valid? x y)
  (and (>= x 0) (< x (length maze))
       (>= y 0) (< y (length (car maze)))
       (list-ref (list-ref maze x) y)))


(define (solve-maze x y path dir*)
  (define ans '())
  (with-handlers ([exn:fail:amb? void])
    (parameterize ([current-amb-tree raise-amb-error])
      (let loop ([x x] [y y] [path path] [dir* dir*])
        (define pos (cons x y))
        (cond
          [(or (member pos path) (not (valid? x y))) (amb)]
          [(eq? '** (list-ref (list-ref maze x) y))
           (set! ans (cons (cons (reverse path) (reverse dir*)) ans))
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
