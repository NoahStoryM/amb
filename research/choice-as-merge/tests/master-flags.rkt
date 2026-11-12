#lang racket/base

;; The two strategy flags of amb as installed (private/amb.rkt), seen
;; from the kernel.  They say where a tree goes in one global frontier;
;; a merge says how a choice combines its own alternatives.
;;
;;   current-amb-depth-first? #f   is a pause at every choice, with
;;                                 amb-level: breadth-first by choices
;;   current-amb-fair? #t          is no merge: the examples of
;;                                 tests/amb.rkt need two different ones

(require rackunit
         (prefix-in m: amb)
         "../kernel.rkt")

;; ---- depth-first? #f ----

(define-syntax-rule (amb/bfs e ...)
  (begin (amb-pause) (amb* amb-level (list (λ () e) ...))))

(test-case "breadth-first search is a pause at every choice"
  (check-equal?
   (parameterize ([m:current-amb-depth-first? #f])
     (for/list ([x (m:in-amb (m:amb 1 (m:amb 2 3) (m:amb 4 5 6) 7 8))]) x))
   '(1 7 8 2 3 4 5 6))
  (check-equal?
   (for/list ([x (in-amb (amb/bfs 1 (amb/bfs 2 3) (amb/bfs 4 5 6) 7 8))]) x)
   '(1 7 8 2 3 4 5 6))
  ;; the order in which the computation itself runs
  (define (master-trace)
    (define res '())
    (for ([_ (m:in-amb
              (let ([x (m:amb 1 2)])
                (set! res (cons (list x) res))
                (let ([y (m:amb 3 4)])
                  (set! res (cons (list x y) res))
                  (let ([z (m:amb 5 6)])
                    (set! res (cons (list x y z) res))
                    (m:amb)))))])
      (void))
    (reverse res))
  (define (kernel-trace)
    (define res '())
    (for ([_ (in-amb
              (let ([x (amb/bfs 1 2)])
                (set! res (cons (list x) res))
                (let ([y (amb/bfs 3 4)])
                  (set! res (cons (list x y) res))
                  (let ([z (amb/bfs 5 6)])
                    (set! res (cons (list x y z) res))
                    (amb)))))])
      (void))
    (reverse res))
  (check-equal? (kernel-trace)
                (parameterize ([m:current-amb-depth-first? #f]) (master-trace))))

;; ---- fair? #t ----

;; Two readings of "the choice point goes to the back after each
;; alternative": the turn passes on at each answer, or the remaining
;; alternatives wait for a pause (and every other choice merges with
;; amb-level, as in one global frontier).
(define-syntax-rule (amb/alternate e ...)
  (amb* amb-alternate (list (λ () e) ...)))
(define-syntax amb/later
  (syntax-rules ()
    [(_ e) e]
    [(_ e0 e ...) (amb* amb-level (list (λ () e0) (λ () (amb-pause) (amb/later e ...))))]))

(test-case "fair? is a place in the global frontier, not a merge"
  ;; first example of "Test fair search" in tests/amb.rkt
  (define-syntax-rule (nested amb) (amb (amb 1 2 3) (amb 'a 'b 'c)))
  (check-equal? (parameterize ([m:current-amb-fair? #t])
                  (for/list ([x (m:in-amb (nested m:amb))]) x))
                '(1 a 2 b 3 c))
  (check-equal? (for/list ([x (in-amb (nested amb/alternate))]) x)
                '(1 a 2 b 3 c))
  (check-equal? (for/list ([x (in-amb (nested amb/later))]) x)
                '(1 2 a 3 b c))               ; not this one
  ;; second example: a fair choice after a depth-first one
  (define-syntax-rule (after amb1 amb2)
    (let* ([b (list 0 (amb1 1 2 3))]
           [c (append b (list (amb2 4 5 6)))])
      c))
  (define master
    (for/list ([x (m:in-amb
                   (let* ([b (list 0 (m:amb 1 2 3))])
                     (parameterize ([m:current-amb-fair? #t])
                       (append b (list (m:amb 4 5 6))))))])
      x))
  (check-equal? master
                '((0 1 4) (0 2 4) (0 3 4) (0 1 5) (0 2 5) (0 3 5) (0 1 6) (0 2 6) (0 3 6)))
  (define-syntax-rule (amb/level e ...) (amb* amb-level (list (λ () e) ...)))
  (check-equal? (for/list ([x (in-amb (after amb/level amb/later))]) x)
                master)
  (check-equal? (for/list ([x (in-amb (after amb/level amb/alternate))]) x)
                '((0 1 4) (0 1 5) (0 1 6) (0 2 4) (0 2 5) (0 2 6) (0 3 4) (0 3 5) (0 3 6))))   ; nor this
