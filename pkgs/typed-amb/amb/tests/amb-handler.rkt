#lang typed/racket/base

;; Typed `amb-fail` and the `#:empty-handler` handler.
(require typed/amb typed/rackunit)

(struct paused ())
(define PAUSED (paused))
(define (suspend) : Void (when (amb #t #f) (amb)))

;; the sequence type covers the handler's type: no `ann` on the body
(define out : (Listof (U Integer paused))
  (for/list ([x (in-amb/do (begin (suspend) (amb 1 2)) #:empty-handler (λ () PAUSED))]) x))
(check-equal? (length out) 3)
(check-true (paused? (car out)))
(check-equal? (cdr out) '(1 2))

(define out* : (Listof (U Integer paused))
  (for/list ([x (in-amb*/do (λ () (suspend) 1) (λ () PAUSED))]) x))
(check-equal? (length out*) 2)

;; without a handler the element type stays Integer and pauses are invisible
(check-equal? (for/list : (Listof Integer) ([x (in-amb/do (begin (suspend) (amb 1 2)))]) x)
              '(1 2))
;; amb-fail as the handler: (U Integer ⊥) is Integer
(check-equal? (for/list : (Listof Integer) ([x (in-amb*/do (λ () (suspend) 1) amb-fail)]) x)
              '(1))

(check-equal? (for/list : (Listof Integer) ([x (in-amb/do (amb (amb-fail) 3))]) x) '(3))
