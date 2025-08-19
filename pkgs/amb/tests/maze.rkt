#lang racket/base

(require "../main.rkt"
         racket/pretty)


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
       (list-ref (list-ref maze x) y)
       #t))


(define (solve-maze x y path dir*)
  (unless (valid? x y) (amb))
  (define pos (cons x y))
  (when (member pos path) (amb))
  (if (eq? '** (list-ref (list-ref maze x) y))
      (cons (reverse path) (reverse dir*))
      (let-values ([(dir x y)
                    (amb (values 'up    (sub1 x) y)
                         (values 'down  (add1 x) y)
                         (values 'left  x (sub1 y))
                         (values 'right x (add1 y)))])
        (solve-maze x y (cons pos path) (cons dir dir*)))))


(for ([ans (in-amb (solve-maze 0 0 '() '()))])
  (pretty-print ans))
