#lang typed/racket/base

;; Typed local frontiers: `amb/local`, `for/amb/local`, `amb-frontier-length`.
(require typed/amb typed/rackunit)

(define (s1) : Integer (amb/local 1 2 3 (amb) 4 5 6))
(define (s2) : Integer (amb/local 7 8 (amb) 9 (amb)))

(check-equal? (for/list : (Listof (∪ Integer 'pause))
                ([x (in-amb/do (amb/local (s1) (s2)) #:empty-handler (λ () 'pause))])
                x)
              '(1 2 3 pause 7 8 pause 4 5 6 9 pause))
(check-equal? (for/list : (Listof Integer) ([x (in-amb/do (amb/local (s1) (s2)))]) x)
              '(1 2 3 7 8 4 5 6 9))

;; for/amb/local takes the same forms as for/amb
(check-equal? (for/list : (Listof Integer)
                ([x (in-amb/do (for/amb/local : Integer ([s (list s1 s2)]) (s)))])
                x)
              '(1 2 3 7 8 4 5 6 9))
(check-equal? (for/list : (Listof Integer)
                ([x (in-amb/do (for/amb/local : Integer #:length 2 ([s (list s1 s2)]) : Integer (s)))])
                x)
              '(1 2 3 7 8 4 5 6 9))
(check-equal? (for/list : (Listof Integer)
                ([x (in-amb/do (for*/amb/local ([l (list (list s1 s2))] [s l]) : Integer (s)))])
                x)
              '(1 2 3 7 8 4 5 6 9))

;; several values
(check-equal? (for/list : (Listof (List Integer Symbol))
                ([(a b) (in-amb/do (amb/local (values 1 'a) (values 2 'b)))])
                (list a b))
              '((1 a) (2 b)))

;; the length of the current frontier
(check-equal? (for/list : (Listof (List Symbol Integer))
                ([(s n) (in-amb/do (let ([s : Symbol (amb/local 'a 'b)])
                                     (values s (amb-frontier-length))))])
                (list s n))
              '((a 1) (b 1)))
