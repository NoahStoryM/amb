#lang typed/racket/base

(require typed/rackunit racket/sequence typed/amb)

(test-case "typed make-amb supports old and new arities"
  (define pending : (Listof Integer) '(1 2))
  (define actual
    (for/list : (Listof (Pairof Integer Any))
      ([entry (in-amb
               (make-amb
                (λ () (pair? pending))
                (λ ()
                  (define v (car pending))
                  (set! pending (cdr pending))
                  (cons v (current-amb-info)))
                'typed-node))])
      entry))
  (check-equal? actual '((1 . typed-node) (2 . typed-node)))
  (check-equal?
   (sequence-ref (in-amb (make-amb (λ () #t) (λ () 99))) 0)
   99))
