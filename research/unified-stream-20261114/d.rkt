#lang racket/base
(require (for-syntax racket/base syntax/parse)
         racket/control racket/stream racket/include "catenable.rkt")
(provide (all-defined-out))
(include "values-core.inc")
(include "choice-macros.inc")

;; Known pure pause block. Its tail is never examined or called here.
(struct paused (n next)
  #:property prop:procedure
  (λ (self) (values (paused-n self) (paused-next self))))
(define (pause-stream n next)
  (unless (exact-positive-integer? n)
    (raise-argument-error 'pause-stream "exact-positive-integer?" n))
  (paused n next))
(define (after-pause r next)
  (if (= r 1) next (paused (sub1 r) next)))

;; Only fuse rounds whose participants are all known pure paused records.
;; An arbitrary thunk, even one which would return another pause, is a barrier.
(define (finish-round ws)
  (define minimum
    (let loop ([xs ws] [n #f])
      (cond
        [(null? xs) n]
        [(paused? (cat-first xs))
         (loop (cat-rest xs) (if n (min n (paused-n (cat-first xs))) (paused-n (cat-first xs))))]
        [else #f])))
  (cond
    [minimum
     (define remaining
       (let loop ([xs ws])
         (cond
           [(null? xs) '()]
           [else
            (define p (cat-first xs))
            (define rest
              (if (= minimum (paused-n p))
                  (paused-next p)
                  (paused (- (paused-n p) minimum) (paused-next p))))
            (cat-append (cat-of rest) (loop (cat-rest xs)))])))
     (values (add1 minimum)
             (leveled '() (cat-first remaining) (cat-rest remaining)))]
    [else (values 1 (leveled '() (cat-first ws) (cat-rest ws)))]))
