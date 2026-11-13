#lang racket/base

;; ===================================================================
;; The language of The Reasoned Schemer, 2nd ed., on kernel.rkt
;; ===================================================================
;;
;; This is ambKanren's typed/miniKanren/amb/trs2 without the types: a
;; goal is a function Substitution → Substitution that chooses among its
;; answers.  Only the section "Goals" differs from that module:
;;
;;   disj2     a choice of `amb`, merged by `current-amb-merge`
;;   conj2     function composition
;;   defrel    pauses before running the body
;;   ifte      searches for the answers of the test locally and passes
;;   once      the pauses of that search on
;;
;; The interface, walk, unify, and reification follow the official code
;; of the second edition (trs2-impl.scm, Copyright (c) 2018 Daniel P.
;; Friedman, William E. Byrd, Oleg Kiselyov, and Jason Hemann, MIT
;; License), through ambKanren and miniKanren-typed.
;;
;; `current-conj-local?` selects the book's conjunction instead: the
;; first goal is searched locally, as the bind of a stream does.

(require (for-syntax racket/base syntax/parse)
         "kernel.rkt")

(provide (all-defined-out)
         current-amb-merge)


;;; Terms and substitutions

(struct var (name) #:transparent)

(define empty-s #hasheq())

(define (walk v s)
  (if (and (var? v) (hash-has-key? s v))
      (walk (hash-ref s v) s)
      v))

(define (occurs? x v s)
  (let ([v (walk v s)])
    (cond
      [(var? v) (eq? v x)]
      [(pair? v) (or (occurs? x (car v) s) (occurs? x (cdr v) s))]
      [else #f])))

(define (ext-s x v s)
  (if (occurs? x v s) (amb-fail) (hash-set s x v)))

(define (unify u v s)
  (let ([u (walk u s)] [v (walk v s)])
    (cond
      [(eq? u v) s]
      [(var? u) (ext-s u v s)]
      [(var? v) (ext-s v u s)]
      [(and (pair? u) (pair? v))
       (unify (cdr u) (cdr v) (unify (car u) (car v) s))]
      [(equal? u v) s]
      [else (amb-fail)])))


;;; Goals

(define ((== u v) s) (unify u v s))

(define (succeed s) s)

(define (fail s) (amb-fail))

(define ((disj2 g1 g2) s) ((amb g1 g2) s))

(define current-conj-local? (make-parameter #f))

(define ((conj2 g1 g2) s)
  (if (current-conj-local?)
      (g2 (amb-local (g1 s)))
      (g2 (g1 s))))

(define (call/fresh name f) (f (var name)))

(define ((ifte g1 g2 g3) s)
  (define s* (amb-await (amb-reify (λ () (g1 s)))))
  (if (null? s*)
      (g3 s)
      (g2 (amb-reflect s*))))

(define ((once g) s)
  (amb-once (λ () (g s))))


;;; Reification

(define (walk* v s)
  (let ([v (walk v s)])
    (cond
      [(var? v) v]
      [(pair? v) (cons (walk* (car v) s) (walk* (cdr v) s))]
      [else v])))

(define (reify-name n)
  (string->symbol (format "_~a" n)))

(define (reify-s v r)
  (let ([v (walk v r)])
    (cond
      [(var? v) (hash-set r v (reify-name (hash-count r)))]
      [(pair? v) (reify-s (cdr v) (reify-s (car v) r))]
      [else r])))

(define ((reify v) s)
  (let* ([v (walk* v s)]
         [r (reify-s v empty-s)])
    (walk* v r)))

(define (run-goal n g)
  (amb-take n (amb-reify (λ () (g empty-s)))))


;;; Syntax

(define-syntax disj
  (syntax-rules ()
    [(_) fail]
    [(_ g) g]
    [(_ g0 g ...) (disj2 g0 (disj g ...))]))

(define-syntax conj
  (syntax-rules ()
    [(_) succeed]
    [(_ g) g]
    [(_ g0 g ...) (conj2 g0 (conj g ...))]))

;; An argument may be written [x : type], as in the typed modules; the
;; type is ignored.
(define-syntax (defrel stx)
  (syntax-parse stx
    #:datum-literals (:)
    [(_ (name:id (~or* x:id [x:id : _]) ...) g ...)
     #'(define (name x ...)
         (λ (s)
           (amb-pause)
           ((conj g ...) s)))]))

(define-syntax run
  (syntax-rules ()
    [(_ n (x0 x ...) g ...)
     (run n q (fresh (x0 x ...)
                (== (list x0 x ...) q)
                g ...))]
    [(_ n q g ...)
     (let ([q (var 'q)])
       (map (reify q) (run-goal n (conj g ...))))]))

(define-syntax-rule (run* q g ...) (run #f q g ...))

(define-syntax fresh
  (syntax-rules ()
    [(_ () g ...) (conj g ...)]
    [(_ (x0 x ...) g ...)
     (call/fresh 'x0
                 (λ (x0)
                   (fresh (x ...) g ...)))]))

(define-syntax-rule (conde [g ...] ...)
  (disj (conj g ...) ...))

(define-syntax conda
  (syntax-rules ()
    [(_ [g0 g ...]) (conj g0 g ...)]
    [(_ [g0 g ...] ln ...)
     (ifte g0 (conj g ...) (conda ln ...))]))

(define-syntax-rule (condu [g0 g ...] ...)
  (conda [(once g0) g ...] ...))

(define-syntax-rule (project (x ...) g ...)
  (λ (s)
    (let ([x (walk* x s)] ...)
      ((conj g ...) s))))
