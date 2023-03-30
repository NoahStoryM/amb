#lang racket

(require data/queue "../main.rkt")

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
       (list-ref (list-ref maze x) y)
       #t))


(define (solve-maze x y path dir*)
  (define ans '())
  (with-handlers ([exn:fail:contract? void])
    (parameterize ([current-amb-queue (make-queue)])
      (let loop ([x x] [y y] [path path] [dir* dir*])
        (unless (valid? x y) (amb))
        (define pos (cons x y))
        (when (member pos path) (amb))
        (cond
          [(eq? '** (list-ref (list-ref maze x) y))
           (define res (cons (reverse path) (reverse dir*)))
           (set! ans (cons res ans))
           (amb)]
          [else
           (let-values ([(dir x y)
                         (amb (values 'up    (sub1 x) y)
                              (values 'down  (add1 x) y)
                              (values 'left  x (sub1 y))
                              (values 'right x (add1 y)))])
             (loop x y (cons pos path) (cons dir dir*)))]))))
  ans)

(pretty-print (solve-maze 0 0 '() '()))
