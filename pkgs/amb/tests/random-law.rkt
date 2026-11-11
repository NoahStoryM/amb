#lang racket/base

;; Random tests of the scheduling of local frontiers.
;;
;; Two properties are checked on randomly generated programs, under the
;; four search strategies:
;;
;;   1. The law: `(k (choice a b))` and `(choice (k a) (k b))` produce
;;      the same results, pauses included, in the same order, where
;;      `choice` is `amb`, `amb/local` or `for/amb/local`, in any context.
;;
;;   2. Collapsing is not observable: a program gives the same results
;;      when each `amb/local` is written as
;;      `(parameterize ([current-amb-frontier (make-amb-frontier)]) (amb ...))`,
;;      whose frontier is exposed and therefore never collapsed.
;;
;; A failure is shrunk and reported with the seed.  The environment
;; variables AMB_RANDOM_N (programs per property) and AMB_RANDOM_SEED
;; override the defaults.
;;
;; Programs are specs, interpreted by `interp` with an input value `v`:
;;
;;   (val c)           the leaf 10v + c
;;   (hole)            the spec in `current-hole` (contexts only)
;;   (yield)           (amb)
;;   (fail)            (amb-fail)
;;   (pause e)         (when (amb #t #f) (amb)), then e
;;   (plain e ...)     (amb e ...), at most three alternatives
;;   (local e ...)     (amb/local e ...), likewise
;;   (for e ...)       (for/amb/local ([x (list e ...)]) x)
;;   (for/len e ...)   the same with #:length
;;   (df b e)          e with current-amb-depth-first? set to b
;;   (fair b e)        e with current-amb-fair? set to b
;;   (bind e k)        k with the result of e as its input

(require rackunit racket/match racket/list amb)

(define n-programs
  (cond [(getenv "AMB_RANDOM_N") => string->number] [else 3000]))
(define seed
  (cond [(getenv "AMB_RANDOM_SEED") => string->number] [else 20261111]))

;; ---- Interpreter ----

(define current-hole (make-parameter #f))
;; #t: `local` is `amb/local`; #f: the `parameterize` form
(define current-local-marked? (make-parameter #t))

(define (pause) (when (amb #t #f) (amb)))

(define-syntax-rule (local-choice e ...)
  (if (current-local-marked?)
      (amb/local e ...)
      (parameterize ([current-amb-frontier (make-amb-frontier)])
        (amb e ...))))

(define (interp s v)
  (match s
    [`(val ,c) (+ (* 10 v) c)]
    [`(hole) (interp (current-hole) v)]
    [`(yield) (amb)]
    [`(fail) (amb-fail)]
    [`(pause ,e) (pause) (interp e v)]
    [`(plain) (amb)]
    [`(plain ,a) (amb (interp a v))]
    [`(plain ,a ,b) (amb (interp a v) (interp b v))]
    [`(plain ,a ,b ,c) (amb (interp a v) (interp b v) (interp c v))]
    [`(local) (local-choice)]
    [`(local ,a) (local-choice (interp a v))]
    [`(local ,a ,b) (local-choice (interp a v) (interp b v))]
    [`(local ,a ,b ,c) (local-choice (interp a v) (interp b v) (interp c v))]
    [`(for ,es ...) (for/amb/local ([e (in-list es)]) (interp e v))]
    [`(for/len ,es ...) (for/amb/local #:length (length es) ([e (in-list es)]) (interp e v))]
    [`(df ,b ,e) (parameterize ([current-amb-depth-first? b]) (interp e v))]
    [`(fair ,b ,e) (parameterize ([current-amb-fair? b]) (interp e v))]
    [`(bind ,e ,k) (let ([x (interp e v)]) (interp k x))]))

;; The results of `s` (pauses are 'P), with `hole` for its hole.
(define (run s [hole #f] #:limit [limit 500])
  (parameterize ([current-hole hole])
    (for/list ([x (in-amb/do (interp s 0) #:empty-handler (λ () 'P))]
               [_ (in-range limit)])
      x)))

(define strategies '((#t #f) (#t #t) (#f #f) (#f #t)))
(define-syntax-rule (with-strategy st e)
  (parameterize ([current-amb-depth-first? (car st)]
                 [current-amb-fair? (cadr st)])
    e))


;; ---- Generator ----

(define (gen-list d) (for/list ([_ (in-range (random 4))]) (gen (sub1 d))))

(define (gen d)
  (case (random (if (<= d 0) 4 20))
    [(0 1) `(val ,(random 10))]
    [(2) `(yield)]
    [(3) (if (zero? (random 3)) `(fail) `(val ,(random 10)))]
    [(4 5) `(pause ,(gen (sub1 d)))]
    [(6 7) `(plain ,@(gen-list d))]
    [(8 9 10 11) `(local ,@(gen-list d))]
    [(12) `(for ,@(gen-list d))]
    [(13) `(for/len ,@(gen-list d))]
    [(14 15 16) `(bind ,(gen (sub1 d)) ,(gen (sub1 d)))]
    [(17) `(df ,(zero? (random 2)) ,(gen (sub1 d)))]
    [(18) `(fair ,(zero? (random 2)) ,(gen (sub1 d)))]
    [else `(local ,(gen (sub1 d)) ,(gen (sub1 d)))]))

;; A spec with exactly one hole.
(define (gen-context d)
  (define (around head)
    (define l (for/list ([_ (in-range (random 3))]) (gen 2)))
    (define-values (before after) (split-at l (random (add1 (length l)))))
    `(,head ,@before ,(gen-context (sub1 d)) ,@after))
  (cond
    [(or (<= d 0) (zero? (random 4))) `(hole)]
    [else
     (case (random 6)
       [(0) `(pause ,(gen-context (sub1 d)))]
       [(1) (around 'plain)]
       [(2 3) (around 'local)]
       [(4) `(bind ,(gen-context (sub1 d)) ,(gen 2))]
       [else `(bind ,(gen 2) ,(gen-context (sub1 d)))])]))


;; ---- Shrinking ----

(define (has-hole? s)
  (and (pair? s) (or (eq? (car s) 'hole) (ormap has-hole? (cdr s)))))

;; The one-step simplifications of a spec; a hole is never dropped.
(define (shrinks s)
  (match s
    [`(val ,c) (if (zero? c) '() '((val 0)))]
    [`(hole) '()]
    [(list (and head (or 'plain 'local 'for 'for/len)) es ...)
     (append
      (for/list ([e (in-list es)] #:unless (has-hole? e)) (cons head (remq e es)))
      (if (= (length es) 1) (list (car es)) '())
      (if (ormap has-hole? es) '() '((val 0)))
      (if (memq head '(for for/len)) (list (cons 'local es)) '())
      (for*/list ([(e i) (in-indexed es)] [e2 (in-list (shrinks e))])
        (cons head (list-set es i e2))))]
    [`(bind ,e ,k)
     (append (if (has-hole? k) '() (list e))
             (if (has-hole? e) '() (list k))
             (for/list ([e2 (in-list (shrinks e))]) `(bind ,e2 ,k))
             (for/list ([k2 (in-list (shrinks k))]) `(bind ,e ,k2)))]
    [`(pause ,e) (cons e (for/list ([e2 (in-list (shrinks e))]) `(pause ,e2)))]
    [(list (and head (or 'df 'fair)) b e)
     (cons e (for/list ([e2 (in-list (shrinks e))]) (list head b e2)))]
    [_ (if (has-hole? s) '() '((val 0)))]))

;; `failure` maps a list of specs to #f or a description of how the
;; property fails; `shrink` simplifies the specs, one at a time, as long
;; as it still fails, and returns the last description.
(define (shrink failure specs)
  (let loop ([specs specs])
    (or (for*/or ([(s i) (in-indexed specs)] [s2 (in-list (shrinks s))])
          (define specs2 (list-set specs i s2))
          (and (failure specs2) (loop specs2)))
        (failure specs))))

(define (check-property name gen-specs failure)
  (random-seed seed)
  (for ([i (in-range n-programs)])
    (define specs (gen-specs))
    (when (failure specs)
      (fail-check (format "~a fails (seed ~a, program ~a); shrunk:\n~a"
                          name seed i (shrink failure specs))))))


;; ---- Property 1: the law ----

(define ((law-failure choice) specs)
  (match-define (list context a b k) specs)
  (define grouped `(bind (,choice ,a ,b) ,k))
  (define distributed `(,choice (bind ,a ,k) (bind ,b ,k)))
  (for/or ([st (in-list strategies)])
    (define o1 (with-strategy st (run context grouped)))
    (define o2 (with-strategy st (run context distributed)))
    (and (not (equal? o1 o2))
         (format "  depth-first? ~a, fair? ~a\n  context: ~s\n  ~s\n    => ~s\n  ~s\n    => ~s"
                 (car st) (cadr st) context grouped o1 distributed o2))))

(for ([choice (in-list '(local plain for for/len))])
  (test-case (format "the law: (k (~a a b)) = (~a (k a) (k b))" choice choice)
    (check-property
     (format "the law for ~a" choice)
     (λ () (list (gen-context 3) (gen 3) (gen 3) (gen 3)))
     (law-failure choice))))


;; ---- Property 2: collapsing is not observable ----

(define (collapse-failure specs)
  (define s (car specs))
  (for/or ([st (in-list strategies)])
    (define o1 (with-strategy st (run s)))
    (define o2 (with-strategy st (parameterize ([current-local-marked? #f]) (run s))))
    (and (not (equal? o1 o2))
         (format "  depth-first? ~a, fair? ~a\n  ~s\n  amb/local    => ~s\n  parameterize => ~s"
                 (car st) (cadr st) s o1 o2))))

(test-case "amb/local agrees with the parameterize form, which is never collapsed"
  (check-property "collapse" (λ () (list (gen 5))) collapse-failure))

;; Unbounded shapes, where chains of finished choices are collapsed all
;; the time.
(test-case "collapsing is not observable in unbounded searches"
  (define (count-up n)
    (local-choice n (begin (pause) (count-up (add1 n)))))
  (define (ticks name)
    (local-choice name (begin (pause) (ticks name))))
  (define (mix n)
    (let ([v (local-choice n (begin (pause) (mix (add1 n))))])
      (amb v (begin (pause) (- v)))))
  (define (nest n)
    (local-choice (local-choice n (begin (pause) (nest (* 2 n))))
                  (begin (pause) (pause) (nest (add1 (* 2 n))))))
  (define (after n)
    (let ([v (local-choice n (begin (pause) (after (add1 n))))])
      (pause)
      (local-choice v (begin (pause) (list v)))))
  (define shapes
    (list (cons 'count-up (λ () (count-up 0)))
          (cons 'ticks (λ () (local-choice (ticks 'a) (local-choice (ticks 'b) (ticks 'c)))))
          (cons 'mix (λ () (mix 0)))
          (cons 'nest (λ () (nest 1)))
          (cons 'after (λ () (after 0)))
          (cons 'all (λ () (local-choice (after 0) (mix 100) (nest 1))))))
  (define (take thk)
    (for/list ([x (in-amb/do (thk) #:empty-handler (λ () 'P))] [_ (in-range 300)]) x))
  (for* ([shape (in-list shapes)] [st (in-list strategies)])
    (check-equal? (with-strategy st (take (cdr shape)))
                  (with-strategy st (parameterize ([current-local-marked? #f]) (take (cdr shape))))
                  (format "~a, depth-first? ~a, fair? ~a" (car shape) (car st) (cadr st)))))
