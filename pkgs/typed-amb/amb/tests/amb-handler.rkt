#lang typed/racket/base

;; Typed `amb-fail`, `amb-pause` and the `#:on-pause` handler.
;;
;; Ported from the v2 tests of the `#:empty-handler` handler.  A pause
;; was (when (amb #t #f) (amb)), answered by the empty handler of the
;; search; it is now (amb-pause), and the handler of #:on-pause is
;; called by the observer.  The cases whose v2 expectation no longer
;; holds are marked CONFLICT; the numbers are those of the untyped
;; amb/tests/v2-conflicts.rkt.
(require typed/amb typed/rackunit)

(struct paused ())
(define PAUSED (paused))
(define (suspend) : Void (amb-pause))

;; the sequence type covers the handler's type: no `ann` on the body
(define out : (Listof (∪ Integer paused))
  (for/list ([x (in-amb/do (begin (suspend) (amb 1 2)) #:on-pause (λ () PAUSED))]) x))
(check-equal? (length out) 3)
(check-true (paused? (car out)))
(check-equal? (cdr out) '(1 2))

(define out* : (Listof (∪ Integer paused))
  (for/list ([x (in-amb*/do (λ () (suspend) 1) (λ () PAUSED))]) x))
(check-equal? (length out*) 2)

;; without a handler the element type stays Integer and pauses are invisible
(check-equal? (for/list : (Listof Integer) ([x (in-amb/do (begin (suspend) (amb 1 2)))]) x)
              '(1 2))

(check-equal? (for/list : (Listof Integer) ([x (in-amb/do (amb (amb-fail) 3))]) x) '(3))

;; CONFLICT (5): a for/amb with no alternative at all fails, with or
;; without #:length; it is not answered by a handler
(check-equal? (for/list : (Listof (∪ Integer paused))
                ([x (in-amb/do (for/amb : Integer ([i : Integer (in-list (ann '() (Listof Integer)))]) i)
                               #:on-pause (λ () PAUSED))])
                x)
              '())
(check-equal? (for/list : (Listof (∪ Integer paused))
                ([x (in-amb/do (for/amb : Integer #:length 0 ([i : Integer (in-list (ann '() (Listof Integer)))]) i)
                               #:on-pause (λ () PAUSED))])
                x)
              '())
(check-equal? (for/list : (Listof (∪ Integer paused))
                ([x (in-amb/do (for/amb : Integer ([i : Integer (in-list '(1 2))]) i) #:on-pause (λ () PAUSED))]) x)
              '(1 2))

;; an empty for/amb evaluates its clauses once, even with a handler
(define n : Integer 0)
(define src : (Listof Integer) '())
(check-equal? (for/list : (Listof (∪ Integer paused))
                ([x (in-amb/do (for/amb : Integer ([i : Integer (begin (set! n (add1 n))
                                                                        (begin0 src (set! src '(1 2))))])
                                 i)
                               #:on-pause (λ () PAUSED))])
                x)
              '())
(check-equal? n 1)

;; several values: body and handler agree
(check-equal? (for/list : (Listof (List Integer Symbol))
                ([(a b) (in-amb/do (begin (suspend) (values 1 'x)) #:on-pause (λ () (values 0 'p)))])
                (list a b))
              '((0 p) (1 x)))

;; CONFLICT (5): (amb) in a for/amb body fails.  With a pause instead,
;; and a handler whose type is a union:
(check-equal? (for/list : (Listof (∪ Integer Symbol))
                ([x (in-amb/do (for/amb : Integer ([i : Integer '(1 2 3)]) (if (odd? i) (amb) i))
                               #:on-pause (λ () : (∪ Symbol Integer) 'odd))])
                x)
              '(2))
(check-equal? (for/list : (Listof (∪ Integer Symbol))
                ([x (in-amb/do (for/amb : Integer ([i : Integer '(1 2 3)]) (when (odd? i) (amb-pause)) i)
                               #:on-pause (λ () : (∪ Symbol Integer) 'odd))])
                x)
              '(odd 1 2 odd 3))

;; CONFLICT (8): there is no empty handler to reenter.  An effect in a
;; clause of a for/amb without #:length is an error of the supply.
(check-exn #rx"not allowed while the alternatives of a choice are supplied"
           (λ ()
             (for/list : (Listof Any)
               ([x (in-amb/do (for/amb : Integer ([i : Integer (in-range (amb 1 2))]) i))])
               x)))
