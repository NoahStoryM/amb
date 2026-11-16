#lang racket/base

;; ===================================================================
;; The four standard mergers, as their equations
;; ===================================================================
;;
;; This is the specification of amb/merge (V3-PLAN section 5): a merger
;; is a closure around its two generators, and nothing else.  merge.rkt
;; compares amb/merge with it.

(provide amb-append amb-interleave amb-alternate amb-level)

(define (pause-count? v) (and (exact-integer? v) (positive? v)))
(define (pauses n g) (if (zero? n) g (λ () (values 1 (pauses (sub1 n) g)))))
(define (bad who v)
  (raise-argument-error who "(or/c list? exact-positive-integer? #f)" v))

;;   (a : s) ∨ t = a : (s ∨ t)
;;   n s     ∨ t = n (s ∨ t)
;;   end     ∨ t = t
(define ((amb-append s t))
  (let-values ([(v s) (s)])
    (cond [(or (list? v) (pause-count? v)) (values v (amb-append s t))]
          [(not v) (t)]
          [else (bad 'amb-append v)])))

;;   pause s ∨ t = pause (t ∨ s)
(define ((amb-interleave s t))
  (let-values ([(v s) (s)])
    (cond [(list? v) (values v (amb-interleave s t))]
          [(pause-count? v) (values 1 (amb-interleave t (pauses (sub1 v) s)))]
          [(not v) (t)]
          [else (bad 'amb-interleave v)])))

;;   (a : s) ∨ t = a : (t ∨ s)
;;   pause s ∨ t = pause (t ∨ s)
(define ((amb-alternate s t))
  (let-values ([(v s) (s)])
    (cond [(list? v) (values v (amb-alternate t s))]
          [(pause-count? v) (values 1 (amb-alternate t (pauses (sub1 v) s)))]
          [(not v) (t)]
          [else (bad 'amb-alternate v)])))

;;   pause s ∨ (b : t) = b : (pause s ∨ t)
;;   pause s ∨ pause t = pause (s ∨ t)
;;   pause s ∨ end     = pause s
(define ((amb-level s t))
  (let-values ([(v s) (s)])
    (cond
      [(list? v) (values v (amb-level s t))]
      [(pause-count? v)
       (let scan ([t t])
         (let-values ([(u t) (t)])
           (cond
             [(list? u) (values u (λ () (scan t)))]
             [(pause-count? u)
              (values 1 (amb-level (pauses (sub1 v) s) (pauses (sub1 u) t)))]
             [(not u) (values 1 (pauses (sub1 v) s))]
             [else (bad 'amb-level u)])))]
      [(not v) (t)]
      [else (bad 'amb-level v)])))
