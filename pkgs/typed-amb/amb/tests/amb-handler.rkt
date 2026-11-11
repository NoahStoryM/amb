#lang typed/racket/base

;; Typed `amb-fail` and the `#:empty-handler` handler.
(require typed/amb typed/rackunit)

(struct paused ())
(define PAUSED (paused))
(define (suspend) : Void (when (amb #t #f) (amb)))

;; the sequence type covers the handler's type: no `ann` on the body
(define out : (Listof (∪ Integer paused))
  (for/list ([x (in-amb/do (begin (suspend) (amb 1 2)) #:empty-handler (λ () PAUSED))]) x))
(check-equal? (length out) 3)
(check-true (paused? (car out)))
(check-equal? (cdr out) '(1 2))

(define out* : (Listof (∪ Integer paused))
  (for/list ([x (in-amb*/do (λ () (suspend) 1) (λ () PAUSED))]) x))
(check-equal? (length out*) 2)

;; without a handler the element type stays Integer and pauses are invisible
(check-equal? (for/list : (Listof Integer) ([x (in-amb/do (begin (suspend) (amb 1 2)))]) x)
              '(1 2))
;; amb-fail as the handler: (∪ Integer ⊥) is Integer
(check-equal? (for/list : (Listof Integer) ([x (in-amb*/do (λ () (suspend) 1) amb-fail)]) x)
              '(1))

(check-equal? (for/list : (Listof Integer) ([x (in-amb/do (amb (amb-fail) 3))]) x) '(3))

;; a for/amb with no alternative at all is (amb), with or without #:length
(define (all-paused? [l : (Listof (∪ Integer paused))]) : Boolean
  (and (pair? l) (andmap paused? l)))
(check-true (all-paused?
             (for/list ([x (in-amb/do (for/amb : Integer ([i : Integer (in-list (ann '() (Listof Integer)))]) i)
                                      #:empty-handler (λ () PAUSED))])
               x)))
(check-true (all-paused?
             (for/list ([x (in-amb/do (for/amb : Integer #:length 0 ([i : Integer (in-list (ann '() (Listof Integer)))]) i)
                                      #:empty-handler (λ () PAUSED))])
               x)))
(check-equal? (for/list : (Listof (∪ Integer paused))
                ([x (in-amb/do (for/amb : Integer ([i : Integer (in-list '(1 2))]) i) #:empty-handler (λ () PAUSED))]) x)
              '(1 2))

;; an empty for/amb evaluates its clauses once, even with a handler
(define n : Integer 0)
(define src : (Listof Integer) '())
(check-true (all-paused?
             (for/list ([x (in-amb/do (for/amb : Integer ([i : Integer (begin (set! n (add1 n))
                                                                               (begin0 src (set! src '(1 2))))])
                                        i)
                                      #:empty-handler (λ () PAUSED))])
               x)))
(check-equal? n 1)

;; several values: body and handler agree, or the handler is amb-fail
(check-equal? (for/list : (Listof (List Integer Symbol))
                ([(a b) (in-amb/do (begin (suspend) (values 1 'x)) #:empty-handler (λ () (values 0 'p)))])
                (list a b))
              '((0 p) (1 x)))
(check-equal? (for/list : (Listof (List Integer Symbol))
                ([(a b) (in-amb*/do (λ () (suspend) (values 1 'x)) amb-fail)])
                (list a b))
              '((1 x)))

;; (amb) in a for/amb body; a handler whose type is a union
(check-equal? (for/list : (Listof (∪ Integer Symbol))
                ([x (in-amb/do (for/amb : Integer ([i : Integer '(1 2 3)]) (if (odd? i) (amb) i))
                               #:empty-handler (λ () : (∪ Symbol Integer) 'odd))])
                x)
              '(odd 2 odd))

;; (amb) reached in the handler (here through an empty for/amb) is a
;; recursive call of the handler; it ends when the handler stops
(define calls : Integer 0)
(check-equal? (for/list : (Listof (∪ Integer Symbol))
                ([x (in-amb/do (amb)
                               #:empty-handler
                               (λ () : (∪ Integer Symbol)
                                 (set! calls (add1 calls))
                                 (if (> calls 3)
                                     'done
                                     (for/amb : Integer ([i : Integer (in-list (ann '() (Listof Integer)))]) i))))])
                x)
              '(done))
(check-equal? calls 4)
