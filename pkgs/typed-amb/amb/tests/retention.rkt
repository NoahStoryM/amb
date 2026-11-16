#lang typed/racket/base

;; The typed front end shares the runtime of the untyped syntax
;; (amb/tests/retention.rkt): a supplied alternative of `amb` or of
;; `for/amb #:length` is not retained by its choice.  The reference to
;; the big vector is one that the compiler cannot drop.
(require typed/amb typed/amb/merge typed/rackunit)

(define mergers : (Listof Merger)
  (list amb-append amb-interleave amb-alternate amb-level))

(define sink : (Boxof Integer) (box 0))
(define wb : (Weak-Boxof (Vectorof Integer)) (make-weak-box (make-vector 0 0)))

(: big! (→ (Vectorof Integer)))
(define (big!)
  (define big : (Vectorof Integer) (make-vector 1000000 1))
  (set! wb (make-weak-box big))
  big)

(: use (→ (Vectorof Integer) Nothing))
(define (use big)
  (set-box! sink (vector-ref big (unbox sink)))
  (amb-fail))

(: probe (→ Symbol))
(define (probe)
  (collect-garbage)
  (collect-garbage)
  (if (weak-box-value wb) 'retained 'freed))

(: answers (→ (→ Symbol) (Listof Symbol)))
(define (answers thk) (for/list : (Listof Symbol) ([x (in-amb* thk)]) x))

(define-type Item (∪ (Vectorof Integer) 'small))

(test-case "a supplied alternative is not retained by its choice"
  (for ([m (in-list mergers)])
    (parameterize ([current-amb-merger m])
      (check-equal? (answers (λ () (let ([big (big!)]) (amb (use big) (probe)))))
                    '(freed))
      (check-equal? (answers (λ () (let ([big (big!)]) (amb (use big) (probe) (probe)))))
                    '(freed freed))
      (check-equal? (answers (λ () (for/amb : Symbol #:length 2
                                            ([b : Item (in-list (list (big!) 'small))])
                                     (if (eq? b 'small) (probe) (use b)))))
                    '(freed))
      (check-equal? (answers (λ () (for/amb : Symbol #:length 3
                                            ([b : Item (in-list (list (big!) 'small 'small))])
                                     (if (eq? b 'small) (probe) (use b)))))
                    '(freed freed))
      (check-equal? (answers (λ () (for/amb : Symbol #:length 2 #:fill (probe)
                                            ([b : (Vectorof Integer) (in-list (list (big!)))])
                                     (use b))))
                    '(freed))
      (check-equal? (answers (λ () (for/amb : Symbol
                                            ([b : Item (in-list (list (big!) 'small))])
                                     (if (eq? b 'small) (probe) (use b)))))
                    '(freed)))))
