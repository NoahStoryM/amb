#lang typed/racket/base

;; Typed `current-amb-local?`: local frontiers.
(require typed/amb typed/rackunit)

(define-syntax-rule (local e) (parameterize ([current-amb-local? #t]) e))

(define (s1) : Integer (local (amb 1 2 3 (amb) 4 5 6)))
(define (s2) : Integer (local (amb 7 8 (amb) 9 (amb))))

(check-equal? (for/list : (Listof (∪ Integer 'pause))
                ([x (in-amb/do (local (amb (s1) (s2))) #:empty-handler (λ () 'pause))])
                x)
              '(1 2 3 pause 7 8 pause 4 5 6 9 pause))
(check-equal? (for/list : (Listof Integer) ([x (in-amb/do (local (amb (s1) (s2))))]) x)
              '(1 2 3 7 8 4 5 6 9))

;; for/amb and make-amb take the parameter too
(check-equal? (for/list : (Listof Integer)
                ([x (in-amb/do (local (for/amb : Integer ([s (list s1 s2)]) (s))))])
                x)
              '(1 2 3 7 8 4 5 6 9))
(check-equal? (for/list : (Listof Integer)
                ([x (in-amb/do
                     (let ([l : (Listof (→ Integer)) (list s1 s2)])
                       ((local ((inst make-amb (→ Integer))
                                (λ () (pair? l))
                                (λ () (begin0 (car l) (set! l (cdr l)))))))))])
                x)
              '(1 2 3 7 8 4 5 6 9))

(check-false (current-amb-local?))
