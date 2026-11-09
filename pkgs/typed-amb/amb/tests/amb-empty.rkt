#lang typed/racket/base

;; RESEARCH (ambKanren issue #14): typed `amb-fail`, `amb-empty`, `#:empty-values`.
(require typed/amb typed/rackunit)

(struct paused ())
(define PAUSED (paused))
(define (suspend) : Void (when (amb #t #f) (amb-empty)))

;; the sequence type covers the empty values: no `ann` on the body
(define out : (Listof (U Integer paused))
  (for/list ([x (in-amb/do (begin (suspend) (amb 1 2)) #:empty-values (list PAUSED))]) x))
(check-equal? (length out) 3)
(check-true (paused? (car out)))
(check-equal? (cdr out) '(1 2))

(define out* : (Listof (U Integer paused))
  (for/list ([x (in-amb*/do (λ () (suspend) 1) (list PAUSED))]) x))
(check-equal? (length out*) 2)

;; without empty values the element type stays Integer and pauses are invisible
(check-equal? (for/list : (Listof Integer) ([x (in-amb/do (begin (suspend) (amb 1 2)))]) x)
              '(1 2))

(check-equal? (for/list : (Listof Integer) ([x (in-amb/do (amb (amb-fail) 3))]) x) '(3))
