#lang racket/load

(require racket/contract/combinator
         racket/mutable-treelist
         racket/set
         racket/stream
         data/queue
         rackunit)
(require "../main.rkt")

(displayln "Test `mtreelist.rkt'")

(define (mutable-treelist-pop! mtl)
  (begin0 (mutable-treelist-last mtl)
    (mutable-treelist-delete! mtl (sub1 (mutable-treelist-length mtl)))))

(current-amb-maker  mutable-treelist)
(current-amb-tasks  (mutable-treelist))
(current-amb-length mutable-treelist-length)
(current-amb-pusher mutable-treelist-add!)
(current-amb-popper mutable-treelist-pop!)

(let ()
  (define (thk)
    (let/cc k
      (define res '())
      (define (return) (k (reverse res)))
      (parameterize ([current-amb-empty-handler return])
        (let ([x (amb 1 2)])
          (set! res (cons (list x) res))
          (let ([y (amb 3 4)])
            (set! res (cons (list x y) res))
            (let ([z (amb 5 6)])
              (set! res (cons (list x y z) res))
              (amb)))))))
  (test-case "DFS"
    (parameterize ([current-amb-tasks ((current-amb-maker))])
      (check-equal?
       (thk)
       '((1) (1 3) (1 3 5)
                   (1 3 6)
             (1 4) (1 4 5)
                   (1 4 6)
         (2) (2 3) (2 3 5)
                   (2 3 6)
             (2 4) (2 4 5)
                   (2 4 6))))
    (parameterize ([current-amb-maker make-queue]
                   [current-amb-tasks (make-queue)]
                   [current-amb-length queue-length]
                   [current-amb-pusher enqueue-front!]
                   [current-amb-popper dequeue!])
      (check-equal?
       (thk)
       '((1) (1 3) (1 3 5)
                   (1 3 6)
             (1 4) (1 4 5)
                   (1 4 6)
         (2) (2 3) (2 3 5)
                   (2 3 6)
             (2 4) (2 4 5)
                   (2 4 6)))))
  (test-case "BFS"
    (parameterize ([current-amb-shuffler void]
                   [current-amb-tasks    ((current-amb-maker))]
                   [current-amb-pusher   mutable-treelist-cons!])
      (check-equal?
       (thk)
       '((1) (2)
         (1 3) (1 4)
         (2 3) (2 4)
         (1 3 5) (1 3 6)
         (1 4 5) (1 4 6)
         (2 3 5) (2 3 6)
         (2 4 5) (2 4 6))))
    (parameterize ([current-amb-shuffler void]
                   [current-amb-maker make-queue]
                   [current-amb-tasks (make-queue)]
                   [current-amb-length queue-length]
                   [current-amb-pusher enqueue!]
                   [current-amb-popper dequeue!])
      (check-equal?
       (thk)
       '((1) (2)
         (1 3) (1 4)
         (2 3) (2 4)
         (1 3 5) (1 3 6)
         (1 4 5) (1 4 6)
         (2 3 5) (2 3 6)
         (2 4 5) (2 4 6))))))

(load "amb.rktl")
