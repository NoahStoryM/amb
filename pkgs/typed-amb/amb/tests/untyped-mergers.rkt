#lang racket/base

;; Mergers from untyped code, for merger-types.rkt.

(require amb amb/merge)

(provide forge first-wins tick-append call-with-forge)

(define (end) (values #f end))

;; makes an answer of its own
(define (forge s t)
  (λ () (values (list "wrong") end)))

;; amb-append, written so that it looks at nothing of a result but
;; whether it is #f or a number
(define ((first-wins s t))
  (let-values ([(v s) (s)])
    (cond
      [(not v) (t)]
      [else (values v (first-wins s t))])))

;; built on a standard merger, which tells answers by `list?`
(define ((tick-append s t)) (values 1 (amb-append s t)))

;; untyped code sets the parameter itself
(define (call-with-forge thk)
  (parameterize ([current-amb-merger forge]) (thk)))
