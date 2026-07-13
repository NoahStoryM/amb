#lang racket/base

;; ============================================================
;; Regression tests: no-#:length for/amb (coroutine path)
;;
;; The no-#:length for/amb defers each iteration body to a thunk that
;; is invoked in the tree-call context.  An earlier stream-based
;; implementation evaluated the body eagerly during stream forcing, so
;; a nested `amb` in the body captured the iteration machinery and lost
;; solutions — test 1 below produced only (-1 1) instead of the full
;; {-2 -1 1 2}.  Tests 5–6 pin down laziness over infinite sequences.
;; ============================================================

(require rackunit racket/stream amb)

(define-syntax-rule (solutions expr)
  (stream->list (in-amb expr)))

;; 1. CORE REGRESSION: nested amb inside a no-#:length for/amb body
(check-equal? (sort (solutions (for/amb ([x '(1 2)]) (amb x (- x)))) <)
              '(-2 -1 1 2)
              "nested amb in no-#:length body must not lose solutions")

;; 2. basic no-#:length enumeration
(check-equal? (solutions (for/amb ([x '(10 20 30)]) x))
              '(10 20 30))

;; 3. #:break stops before the triggering element
(check-equal? (solutions (for/amb ([x '(1 2 3 4 5)]) #:break (> x 3) x))
              '(1 2 3))

;; 4. #:final includes the triggering element
(check-equal? (solutions (for/amb ([x '(1 2 3 4 5 6)]) #:final (>= x 4) x))
              '(1 2 3 4))

;; 5. laziness over an infinite sequence (take 5)
(check-equal? (for/list ([v (in-amb/do (for/amb ([n (in-naturals)]) n))]
                         [_ (in-range 5)])
                v)
              '(0 1 2 3 4))

;; 6. infinite sequence + nested amb in body (take 6)
(check-equal? (sort (for/list ([v (in-amb/do
                                   (for/amb ([n (in-naturals)]) (amb n (- n))))]
                               [_ (in-range 6)])
                      v)
                    <)
              '(-2 -1 0 0 1 2))

;; 7. for*/amb no-#:length + #:break
(check-equal? (solutions (for*/amb ([x '(1 2 3)] [y '(10 20 30)])
                           #:break (and (= x 2) (= y 20))
                           (+ x y)))
              '(11 21 31 12))

;; 8. for*/amb no-#:length + nested amb in body
(check-equal? (sort (solutions (for*/amb ([x '(1 2)]) (+ (* 10 x) (amb 1 2)))) <)
              '(11 12 21 22))

;; 9. #:break on the first element yields nothing
(check-equal? (solutions (for/amb ([x '(1 2 3)]) #:break #t x))
              '())

(displayln "all for/amb regression checks passed")
