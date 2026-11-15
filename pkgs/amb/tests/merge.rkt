#lang racket/base

;; amb/merge against the equations of spec-merge.rkt, in the two senses
;; of V3-PLAN section 3:
;;
;;   the same events   n pauses being n times one pause; up to the end,
;;                     or up to the call that does not return (here: the
;;                     call that raises);
;;   the same calls    before every event, the same generators have been
;;                     called, in the same order.
;;
;; The raw results are not compared: they are not the same.
;;
;; Both sides are given the same leaves, in the same pieces; what is
;; ignored is how often the mergers call each other.  The whole log is
;; compared too, so the calls after the last event count: those before
;; the end, and those before a call that raises.  This is a test of
;; finite runs: a call that does not return is only stood for by one
;; that raises.

(require rackunit
         racket/list
         racket/match
         (prefix-in spec: "spec-merge.rkt")
         (prefix-in flat: "../merge.rkt")
         "../private/data/catenable.rkt")

(define (spec-merge m)
  (case m
    [(append) spec:amb-append]
    [(interleave) spec:amb-interleave]
    [(alternate) spec:amb-alternate]
    [(level) spec:amb-level]))
(define (flat-merge m)
  (case m
    [(append) flat:amb-append]
    [(interleave) flat:amb-interleave]
    [(alternate) flat:amb-alternate]
    [(level) flat:amb-level]))
(define strategies '(append interleave alternate level))

(define (pauses n g) (if (zero? n) g (λ () (values 1 (pauses (sub1 n) g)))))


;; ---- generators that keep a log ----

;; The log of a run: the calls of the leaves, the last one first, and
;; how many they are.
(define log (make-parameter #f))
(define calls (make-parameter #f))
(define (note! x)
  (set-box! (log) (cons x (unbox (log))))
  (set-box! (calls) (add1 (unbox (calls)))))
(define-syntax-rule (with-log e ...)
  (parameterize ([log (box '())] [calls (box 0)]) e ...))

;; A leaf is a list of raw results:
;;   (list v)   an answer
;;   n          n pauses
;;   boom       the call raises: it stands for a call that does not
;;              return
;;   0          not a result: the merger that sees it raises
;; and then it ends.  Nothing may be called after the end.
(define (leaf id raws)
  (let loop ([raws raws] [i 0])
    (λ ()
      (note! (cons id i))
      (match raws
        ['() (values #f (λ () (error "called after the end" id)))]
        [(cons 'boom _) (error "boom" id)]
        [(cons r raws) (values r (loop raws (add1 i)))]))))


;; ---- expressions ----
;;
;;   e ::= (leaf id raws)
;;       | (m e e)            a merge, m one of the four strategies
;;       | (drop k e)         e, after k of its events were taken

(define (build merge-of e)
  (let build ([e e])
    (match e
      [`(leaf ,id ,raws) (leaf id raws)]
      [`(drop ,k ,e) (drop k (build e))]
      [`(,m ,l ,r) ((merge-of m) (build l) (build r))])))

;; What is left of g after k events.
(define (drop k g)
  (if (zero? k)
      g
      (let-values ([(v next) (g)])
        (cond
          [(not v) (λ () (values #f (λ () (error "called after the end"))))]
          [(list? v) (drop (sub1 k) next)]
          [(eqv? v 0) (error "zero")]
          [(<= v k) (drop (- k v) next)]
          [else (pauses (- v k) next)]))))

;; The events of g, each with the number of calls logged when it came:
;; an answer, pause, end; or (raised message) for the call that raised.
(define (observe g fuel)
  (define (stamp e) (cons e (unbox (calls))))
  (let loop ([g g] [fuel fuel])
    (if (zero? fuel)
        '()
        (let-values ([(v next)
                      (with-handlers ([exn:fail? (λ (e) (values 'raised (exn-message e)))])
                        (g))])
          (cond
            [(eq? v 'raised) (list (stamp (list 'raised next)))]
            [(not v) (list (stamp 'end))]
            [(list? v) (cons (stamp v) (loop next (sub1 fuel)))]
            [(eqv? v 0) (list (stamp 'zero))]
            [else (cons (stamp 'pause) (loop (pauses (sub1 v) next) (sub1 fuel)))])))))

;; The events with their stamps, and the log, of the expression e.
(define (run merge-of e fuel)
  (with-log
    (define events
      (with-handlers ([exn:fail? (λ (x) (list 'raised-while-building (exn-message x)))])
        (observe (build merge-of e) fuel)))
    (list events (reverse (unbox (log))))))

(define (events-of result) (map car (car result)))


;; ---- random expressions ----

(define next-id 0)
(define (fresh-id) (set! next-id (add1 next-id)) next-id)

(define (random-raws bombs?)
  (define raws
    (for/list ([_ (random 5)])
      (case (random 8)
        [(0 1 2) (list (random 100))]
        [(3 4 5) 1]
        [else (add1 (random 4))])))
  (if (and bombs? (zero? (random 8)))
      (append raws (list (if (zero? (random 3)) 0 'boom)))
      raws))

(define (random-expression size strategies bombs? [left 3])
  (let gen ([size size])
    (cond
      [(<= size 1) `(leaf ,(fresh-id) ,(random-raws bombs?))]
      [(zero? (random 6)) `(drop ,(random 4) ,(gen size))]
      [else
       ;; mostly nested on the left
       (define l (if (zero? (random left))
                     (add1 (random (sub1 size)))
                     (max 1 (- size 1 (random 2)))))
       (define l* (min l (sub1 size)))
       `(,(list-ref strategies (random (length strategies)))
         ,(gen l*) ,(gen (- size l*)))])))

(define (check-same e fuel)
  (define expected (run spec-merge e fuel))
  (define actual (run flat-merge e fuel))
  (unless (equal? actual expected)
    (fail-check (format "~s\n  fuel ~a\n  equations ~s\n  flat      ~s" e fuel expected actual))))


;; ---- the tests ----

(test-case "examples"
  (define (events m . leaves)
    (events-of
     (run flat-merge
          (for/fold ([e #f]) ([raws leaves] [i (in-naturals)])
            (define l `(leaf ,i ,raws))
            (if e `(,m ,e ,l) l))
          100)))
  (check-equal? (events 'append '((a) 2 (b)) '() '((c) 1))
                '((a) pause pause (b) (c) pause end))
  ;; rounds: 3 pauses are three rounds
  (check-equal? (events 'level '((a) 3 (b)) '((c) 1 (d)) '(1 1 (e) 1))
                '((a) (c) pause (d) pause (e) pause (b) end))
  ;; the last one goes on by itself
  (check-equal? (events 'level '() '(2 (a)) '()) '(pause pause (a) end)))

(test-case "flat = equations: events and calls, only the two flat strategies"
  (for ([m '(append level)])
    (for* ([size '(2 3 5 8 12)] [_ 400])
      (check-same (random-expression size (list m) #f) (random 30)))))

(test-case "flat = equations: the four strategies mixed"
  (for* ([size '(2 3 5 8 12 20)] [_ 500])
    (check-same (random-expression size strategies #f) (random 40))))

(test-case "flat = equations: generators that raise, or return 0"
  (for* ([size '(2 3 5 8 12)] [_ 500])
    (check-same (random-expression size strategies #t) (random 40)))
  (for* ([m '(append level)] [size '(3 6 10)] [_ 300])
    (check-same (random-expression size (list m) #t) (random 40))))

(test-case "nothing is called after the last event that is asked for"
  ;; every leaf ends in a call that raises; with fuel for all the events
  ;; before it, the call is not made
  (for* ([m '(append level)] [_ 300])
    (define e (random-expression 6 (list m) #f))
    (define all (events-of (run spec-merge e 1000)))
    (for ([fuel (in-range (length all))])
      (check-same e fuel))))

(test-case "the events do not depend on how the pauses are written"
  ;; the same events, written in another way: the counts of pauses cut
  ;; up or put together
  (define (rewrite raws)
    (define ones
      (append* (for/list ([r raws])
                 (cond [(list? r) (list r)]
                       [else (make-list r 1)]))))
    (let loop ([ones ones])
      (cond
        [(null? ones) '()]
        [(list? (car ones)) (cons (car ones) (loop (cdr ones)))]
        [else
         (define-values (ps rest) (splitf-at ones (λ (x) (eqv? x 1))))
         (define n (add1 (random (length ps))))
         (cons n (loop (append (make-list (- (length ps) n) 1) rest)))])))
  (define (rewrite-leaves e)
    (match e
      [`(leaf ,id ,raws) `(leaf ,id ,(rewrite raws))]
      [`(drop ,k ,e) `(drop ,k ,(rewrite-leaves e))]
      [`(,m ,l ,r) `(,m ,(rewrite-leaves l) ,(rewrite-leaves r))]))
  (for* ([size '(2 3 5 8 12)] [_ 400])
    (define e (random-expression size strategies #f))
    (check-equal? (events-of (run flat-merge (rewrite-leaves e) 1000))
                  (events-of (run flat-merge e 1000)))))

(test-case "the steps of the lists"
  ;; a merge nested n deep on the left; and a stream that is merged again
  ;; after each of its events
  (define (left m n)
    (for/fold ([g (leaf 0 '((0)))]) ([i (in-range 1 n)])
      ((flat-merge m) g (leaf i (list (list i))))))
  (define (again m n)
    (for/fold ([g (leaf 0 (for/list ([i n]) (list i)))]) ([i n])
      (define-values (v next) (((flat-merge m) g (leaf (- -1 i) '()))))
      next))
  (for* ([m '(append level)] [n '(1000 10000 100000)])
    (with-log
      (define before (cat-steps))
      (check-equal? (for/sum ([e (observe (left m n) (* 2 n))]) (if (pair? (car e)) 1 0)) n)
      (check-true (<= (- (cat-steps) before) (* 4 n))))
    (with-log
      (define before (cat-steps))
      (check-equal? (map car (observe (again m n) 10)) '(end))
      (check-true (<= (- (cat-steps) before) (* 4 n))))))
