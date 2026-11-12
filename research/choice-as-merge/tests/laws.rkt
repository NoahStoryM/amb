#lang racket/base

;; Which laws hold, and how strongly, for each strategy and for the two
;; ways of reading conjunction:
;;
;;   direct   conjunction continues the computation (kernel.rkt)
;;   book     conjunction is the bind of the stream (microKanren, TRS2)
;;
;; Two goals can agree
;;
;;   events   on the whole stream: the same answers and pauses, in order;
;;   answers  on the order of the answers, but not on where the pauses are;
;;   bag      on the answers only, in some order.
;;
;; Each law is tried on random goals, alone and followed by a further
;; goal, and the table records the strongest agreement that held every
;; time.  `expected` is checked; running the module prints the table and,
;; for every law that is not `events`, the smallest counterexample found.

(require racket/list
         racket/match
         rackunit
         "../kernel.rkt"
         "../model.rkt")

(define merges
  (list (cons 'append amb-append)
        (cons 'interleave amb-interleave)
        (cons 'alternate amb-alternate)
        (cons 'level amb-level)))

(define laws
  `([conj-assoc   (seq (seq a b) c)   (seq a (seq b c))]
    [conj-unit-l  (seq succeed a)     a]
    [conj-unit-r  (seq a succeed)     a]
    [conj-zero-l  (seq fail a)        fail]
    [conj-zero-r  (seq a fail)        fail]
    [dist-right   (seq (alt a b) c)   (alt (seq a c) (seq b c))]
    [dist-left    (seq a (alt b c))   (alt (seq a b) (seq a c))]
    [disj-unit-l  (alt fail a)        a]
    [disj-unit-r  (alt a fail)        a]
    [disj-assoc   (alt (alt a b) c)   (alt a (alt b c))]
    [disj-comm    (alt a b)           (alt b a)]
    [local        (local a)           a]
    [local-idem   (local (local a))   (local a)]))

;;                        direct                              book
;;              append interleave alternate level   append interleave alternate level
(define expected
  '([conj-assoc   events events  events  events     events bag     bag     bag]
    [conj-unit-l  events events  events  events     events events  events  events]
    [conj-unit-r  events events  events  events     events events  events  events]
    [conj-zero-l  events events  events  events     events events  events  events]
    [conj-zero-r  answers answers answers answers   answers answers answers answers]
    [dist-right   events events  events  events     events bag     bag     bag]
    [dist-left    bag    bag     bag     bag        bag    bag     bag     bag]
    [disj-unit-l  events events  events  events     events events  events  events]
    [disj-unit-r  events events  events  events     events events  events  events]
    [disj-assoc   events bag     bag     events     events bag     bag     events]
    [disj-comm    bag    bag     bag     bag        bag    bag     bag     bag]
    [local        events bag     bag     bag        events events  events  events]
    [local-idem   events events  events  events     events events  events  events]))

(define (instantiate template env)
  (let loop ([t template])
    (cond
      [(assq t env) => cdr]
      [(pair? t) (cons (loop (car t)) (loop (cdr t)))]
      [else t])))

(define (sort-answers l)
  (sort l string<? #:key (λ (a) (format "~s" a))))

;; The strongest agreement between two event lists.
(define (agreement e1 e2)
  (cond
    [(equal? e1 e2) 'events]
    [(equal? (filter pair? e1) (filter pair? e2)) 'answers]
    [(equal? (sort-answers (filter pair? e1)) (sort-answers (filter pair? e2))) 'bag]
    [else 'none]))

(define rank '(none bag answers events))
(define (weaker a b)
  (if (< (index-of rank a) (index-of rank b)) a b))

;; Goals for a, b, c and the context d, with distinct tags, of total
;; size about `size`.
(define (random-env size)
  (for/list ([x '(a b c d)] [first '(100 200 300 400)])
    (cons x (random-goal (add1 (random size)) '(seq alt) first))))

;; Returns the weakest agreement over all trials, and for each agreement
;; weaker than `events` the smallest instance that showed it.
(define (classify run lhs rhs)
  (define witnesses (make-hasheq))
  (define result
    (for*/fold ([result 'events]) ([size (in-range 1 7)] [_ 400])
      (define env (random-env size))
      (for/fold ([result result]) ([context '(□ (seq □ d))])
        (define l (instantiate (instantiate context `((□ . ,lhs))) env))
        (define r (instantiate (instantiate context `((□ . ,rhs))) env))
        (define el (run l))
        (define er (run r))
        (define a (agreement el er))
        (unless (eq? a 'events)
          (define old (hash-ref witnesses a #f))
          (when (or (not old) (< (goal-size l) (goal-size (car old))))
            (hash-set! witnesses a (list l r el er))))
        (weaker result a))))
  (values result (hash-ref witnesses result #f)))

(module+ test
  (random-seed 20261112)
  (define show? (getenv "LAWS_VERBOSE"))
  (printf "~a  ~a\n" (make-string 12 #\space)
          (apply string-append
                 (for*/list ([mode '(direct book)] [m (map car merges)])
                   (format "~a/~a  " (substring (symbol->string mode) 0 1) m))))
  (for ([law laws] [row expected])
    (match-define (list name lhs rhs) law)
    (define results
      (for*/list ([mode (list direct book)] [m (map cdr merges)])
        (define-values (result witness) (classify (mode m) lhs rhs))
        (cons result witness)))
    (printf "~a ~a\n"
            (let ([s (symbol->string name)])
              (string-append s (make-string (- 12 (string-length s)) #\space)))
            (map car results))
    (check-equal? (map car results) (cdr row) (symbol->string name))
    (when show?
      (for ([r results]
            [label (for*/list ([mode '(direct book)] [m (map car merges)])
                     (format "~a/~a" mode m))])
        (match r
          [(cons 'events _) (void)]
          [(list a l r el er)
           (printf "    ~a: ~a only\n      ~s\n        => ~s\n      ~s\n        => ~s\n"
                   label a l el r er)])))))
