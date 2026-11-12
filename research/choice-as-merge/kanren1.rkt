#lang racket/base

;; ===================================================================
;; The language of The Reasoned Schemer, 1st ed., on kernel.rkt
;; ===================================================================
;;
;; The first edition has two disjunctions and two conjunctions, and no
;; pauses:
;;
;;   conde   depth-first             a choice merged by amb-append
;;   condi   interleaving            a choice merged by amb-alternate
;;   all     depth-first             the first goal is searched locally,
;;   alli    interleaving              and its answers are chosen among
;;                                     with amb-append / amb-alternate
;;
;; Without pauses, a branch gives up its turn only by answering, and an
;; answer of a goal is an event only for a search of that goal alone.
;; This is why both conjunctions search their first goal locally, as the
;; `bind` of the book does: in plain function composition, an answer of
;; the first goal that the second goal rejects is no event at all, and
;; `condi` would never get to pass the turn on.
;;
;; The macros keep the structure of the book's appendix A (Friedman,
;; Byrd, and Kiselyov, MIT Press, 2005), through ambKanren and
;; miniKanren-typed; walk, unify, and reification are those of kanren.rkt
;; with the first edition's names.

(require "kernel.rkt")

(provide (all-defined-out))


;;; Terms and substitutions

(struct var (name) #:transparent)

(define empty-s #hasheq())
(define (size-s s) (hash-count s))

(define (walk v s)
  (if (and (var? v) (hash-has-key? s v))
      (walk (hash-ref s v) s)
      v))

(define (walk* v s)
  (let ([v (walk v s)])
    (cond
      [(var? v) v]
      [(pair? v) (cons (walk* (car v) s) (walk* (cdr v) s))]
      [else v])))

(define (occurs? x v s)
  (let ([v (walk v s)])
    (cond
      [(var? v) (eq? v x)]
      [(pair? v) (or (occurs? x (car v) s) (occurs? x (cdr v) s))]
      [else #f])))

(define (ext-s x v s)
  (if (occurs? x v s) (amb-fail) (hash-set s x v)))

(define (unify v w s)
  (let ([v (walk v s)] [w (walk w s)])
    (cond
      [(eq? v w) s]
      [(var? v) (ext-s v w s)]
      [(var? w) (ext-s w v s)]
      [(and (pair? v) (pair? w))
       (unify (cdr v) (cdr w) (unify (car v) (car w) s))]
      [(equal? v w) s]
      [else (amb-fail)])))

(define (reify-name n)
  (string->symbol (format "_.~a" n)))

(define (reify-s v s)
  (let ([v (walk v s)])
    (cond
      [(var? v) (hash-set s v (reify-name (size-s s)))]
      [(pair? v) (reify-s (cdr v) (reify-s (car v) s))]
      [else s])))

(define (reify v) (walk* v (reify-s v empty-s)))


;;; Goals

(define (succeed s) s)
(define (fail s) (amb-fail))
(define ((== v w) s) (unify v w s))

;; bind and bindi: an answer of (g s), chosen among all of them as the
;; conjunction says.
(define (answer-of g s merge)
  (amb-reflect (amb-reify (λ () (g s))) merge))

(define-syntax all-aux
  (syntax-rules ()
    [(_ merge) succeed]
    [(_ merge g) g]
    [(_ merge g0 g ...)
     (let ([g^ g0])
       (λ (s) ((all-aux merge g ...) (answer-of g^ s merge))))]))
(define-syntax-rule (all  g ...) (all-aux amb-append    g ...))
(define-syntax-rule (alli g ...) (all-aux amb-alternate g ...))

;; mplus and mplusi: a choice.
(define-syntax-rule (if-aux merge g0 g1 g2)
  (λ (s)
    (amb* merge (list (λ () ((all g0 g1) s))
                      (λ () (g2 s))))))
(define-syntax-rule (ife g0 g1 g2) (if-aux amb-append    g0 g1 g2))
(define-syntax-rule (ifi g0 g1 g2) (if-aux amb-alternate g0 g1 g2))

(define-syntax-rule (ifa g0 g1 g2)
  (λ (s)
    (let ([s* (amb-await (amb-reify (λ () (g0 s))))])
      (if (null? s*)
          (g2 s)
          (g1 (amb-reflect s* amb-append))))))

(define-syntax-rule (ifu g0 g1 g2)
  (λ (s)
    (let ([s* (amb-await (amb-reify (λ () (g0 s))))])
      (if (null? s*)
          (g2 s)
          (g1 (caar s*))))))

(define-syntax cond-aux
  (syntax-rules (else)
    [(_ ifer) fail]
    [(_ ifer [else g ...]) (all g ...)]
    [(_ ifer [g ...]) (all g ...)]
    [(_ ifer [g0 g ...] c ...)
     (ifer g0 (all g ...) (cond-aux ifer c ...))]))
(define-syntax-rule (conde c ...) (cond-aux ife c ...))
(define-syntax-rule (condi c ...) (cond-aux ifi c ...))
(define-syntax-rule (conda c ...) (cond-aux ifa c ...))
(define-syntax-rule (condu c ...) (cond-aux ifu c ...))

(define-syntax-rule (fresh (x ...) g ...)
  (λ (s)
    (let ([x (var 'x)] ...)
      ((all g ...) s))))

(define-syntax-rule (project (x ...) g ...)
  (λ (s)
    (let ([x (walk* x s)] ...)
      ((all g ...) s))))

(define-syntax-rule (run n^ (x) g ...)
  (let ([n n^] [x (var 'x)])
    (if (or (not n) (> n 0))
        (map (λ (s) (reify (walk* x s)))
             (amb-take n (amb-reify (λ () ((all g ...) empty-s)))))
        '())))
(define-syntax-rule (run* (x) g ...) (run #f (x) g ...))
