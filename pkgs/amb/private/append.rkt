#lang racket/base

(require "generator.rkt"
         "data/catenable.rkt")

(provide amb-append)

;; Depth-first search: all of s, then t.
;;
;;   (appended s ts)   the generator s, then the generators ts in order
;;
;; The remaining generators are held in a catenable list so nested
;; appends can combine their lists before running another step.
(struct appended (s ts)
  #:property prop:procedure
  (λ (self) (append-run (appended-s self) (appended-ts self))))

(define (amb-append s t)
  (appended s (cat-of t)))

(define (append-run s ts)
  (cond
    [(appended? s)
     (append-run (appended-s s) (cat-append (appended-ts s) ts))]
    ;; nothing comes after s: it goes on by itself
    [(null? ts) (s)]
    [else
     (let-values ([(v s) (s)])
       (cond
         ;; n pauses stay together: no turn passes on at a pause
         [(or (list? v) (exact-positive-integer? v))
          (define right (and (eq? s end) (cat-singleton? ts) (cat-first ts)))
          ;; Keep impersonated generators behind this merger so an outer
          ;; merger cannot fuse them and bypass a wrapper's procedure call.
          (values v (if (and (procedure? right) (not (impersonator? right)))
                        right
                        (appended s ts)))]
         [(not v) (append-run (cat-first ts) (cat-rest ts))]
         [else (raise-result-error 'amb-append v)]))]))
