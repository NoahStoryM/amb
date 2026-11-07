#lang racket/base

(require rackunit racket/list racket/match racket/sequence racket/stream
         amb
         "../private/hierarchical.rkt")

(define (collect source) (sequence->list (search->sequence source)))
(define (run thunk) (collect (search/thunk thunk)))

(check-equal? (run (lambda () (amb (amb) 1 (amb 2 3)))) '(1 2 3))
(check-equal? (run (lambda () (amb (begin (suspend) 'a) 'b))) '(b a))
(check-equal? (run (lambda () (amb (begin (suspend) 'a)
                                  (begin (suspend) 'b)))) '(a b))
(check-equal? (run (lambda () (suspend) (suspend) (amb 1 2))) '(1 2))
(check-equal? (run (lambda () (amb (begin (suspend) (amb)) 9))) '(9))
(check-equal? (run (lambda () (for/amb ([i (in-range 4)])
                               (amb i (- i))))) '(0 0 1 -1 2 -2 3 -3))
(check-equal? (run (lambda () (for/amb #:length 4 ([i (in-range 4)]) i)))
              '(0 1 2 3))
(check-equal? (for/list ([(x y) (search->sequence
                                (search/thunk
                                 (lambda () (suspend)
                                   (amb (values 1 2) (values 3 4)))))])
                (list x y)) '((1 2) (3 4)))
(check-equal? (for/list ([() (search->sequence
                            (search/thunk (lambda () (suspend) (values))))])
                'zero-values) '(zero-values))
(check-exn exn:fail? suspend)
(check-equal? (collect (search-defer (lambda () (search/thunk (lambda () 7)))))
              '(7))

;; The public strategy is selected when a sequence is constructed, even
;; though its body may be forced under a different parameterization.
(define delayed-public
  (parameterize ([current-amb-search-strategy 'hierarchical])
    (in-amb (amb (begin (amb-suspend) 'a) 'b))))
(check-equal? (stream->list delayed-public) '(b a))
(check-equal? (current-amb-search-strategy) 'flat)
(check-exn exn:fail:contract?
           (lambda () (current-amb-search-strategy 'unknown)))
(check-exn #rx"hierarchical"
           (lambda () (stream->list (in-amb (amb-local 1)))))

(for* ([depth-first? '(#f #t)] [fair? '(#f #t)])
  (check-equal?
   (parameterize ([current-amb-search-strategy 'hierarchical]
                  [current-amb-depth-first? depth-first?]
                  [current-amb-fair? fair?])
     (for/list ([x (in-amb
                   (let ([x (amb-local (amb (amb 1 2) 3))])
                     (amb-suspend)
                     x))]) x))
   '(1 2 3)))

(check-equal?
 (parameterize ([current-amb-search-strategy 'hierarchical])
   (for/list ([_ (in-range 6)]
              [x (in-amb/do (for/amb ([i (in-naturals)]) (amb i (- i))))])
     x))
 '(0 0 1 -1 2 -2))

;; A nested flat root must not inherit permission to use local operations
;; from the hierarchical root currently consuming it.
(check-exn
 #rx"hierarchical"
 (lambda ()
   (parameterize ([current-amb-search-strategy 'hierarchical])
     (for/list ([x (in-amb
                   (stream->list
                    (parameterize ([current-amb-search-strategy 'flat])
                      (in-amb (amb-suspend)))))]) x))))

(check-equal?
 (parameterize ([current-amb-search-strategy 'hierarchical])
   (for/list ([x (in-amb
                 (stream->list
                  (parameterize ([current-amb-search-strategy 'flat])
                    (in-amb (amb 1 2)))))]) x))
 '((1 2)))

(define (public-run thunk)
  (parameterize ([current-amb-search-strategy 'hierarchical])
    (for/list ([x (in-amb* thunk)]) x)))
(check-equal? (public-run (lambda () (amb-local #:limit 2 (amb 1 2 3)))) '(1 2))
(check-equal? (public-run (lambda () (amb-local #:limit #f (amb 1 2)))) '(1 2))
(check-equal? (public-run (lambda () (amb-local #:default 9 (amb)))) '(9))
(check-equal? (public-run (lambda () (amb-local #:default (amb 7 8) (amb)))) '(7 8))
(check-equal? (public-run (lambda () (amb-local #:default (error "unused") (amb 1 2))))
              '(1 2))
(check-equal? (public-run (lambda () (amb-local #:limit 0 (error "unused")))) '())
(check-equal? (public-run (lambda ()
                           (amb-local #:limit 0 #:default 9 (error "unused")))) '(9))
(check-equal? (public-run (lambda ()
                           (define x (amb-local #:default #f (amb 1 2)))
                           (if x (amb) 9))) '())
(check-equal? (public-run (lambda ()
                           (define x (amb-local #:limit 1 (amb 1 2)))
                           (if (= x 1) (amb) 9))) '())
(check-equal? (public-run (lambda ()
                           (amb-local #:limit 2
                                      (begin (amb-suspend) (amb 1 2 3))))) '(1 2))
(check-exn exn:fail:contract?
           (lambda () (public-run (lambda () (amb-local #:limit -1 1)))))

(define local-context (make-parameter 'outside))
(define local-mark (make-continuation-mark-key 'local-context))
(define (context-value)
  (list (local-context) (continuation-mark-set-first #f local-mark)))
(check-equal?
 (public-run
  (lambda ()
    (parameterize ([local-context 'inside])
      (with-continuation-mark local-mark 'marked
        (list (amb-local (begin (amb-suspend) (context-value)))
              (context-value))))))
 '(((inside marked) (inside marked))))
(check-equal?
 (public-run
  (lambda ()
    (parameterize ([local-context 'default-context])
      (with-continuation-mark local-mark 'default-mark
        (amb-local #:default (begin (amb-suspend) (context-value)) (amb))))))
 '((default-context default-mark)))

;; The finite oracle deliberately uses eager pairs and ordinary procedures,
;; not the experimental event protocol or the implementation's merge code.
;; Adapted from microKanren and TRS2 via NoahStoryM/miniKanren-typed.
;; See ../NOTICE for source attribution, changes, and MIT license notices.
(define (plus xs ys)
  (cond [(null? xs) ys]
        [(procedure? xs) (lambda () (plus ys (xs)))]
        [else (cons (car xs) (plus (cdr xs) ys))]))
(define (bind xs f)
  (cond [(null? xs) '()]
        [(procedure? xs) (lambda () (bind (xs) f))]
        [else (plus (f (car xs)) (bind (cdr xs) f))]))
(define (pull xs)
  (if (procedure? xs) (pull (xs)) xs))
(define (take-stream xs n)
  (if (zero? n) '()
      (let ([xs (pull xs)])
        (if (null? xs) '()
            (cons (car xs) (take-stream (cdr xs) (sub1 n)))))))

;; Compare suspension boundaries as well as answers: equal final lists alone
;; would not establish that these searches compose correctly inside a merge.
(define (oracle-trace xs)
  (cond [(null? xs) '(done)]
        [(procedure? xs) (cons 'pause (oracle-trace (xs)))]
        [else (cons `(value ,(car xs)) (oracle-trace (cdr xs)))]))
(define (search-trace source)
  (define event (source))
  (cond [(not event) '(done)]
        [(answer? event)
         (cons `(value ,@(answer-values event))
               (search-trace (answer-rest event)))]
        [else (cons 'pause (search-trace (suspension-resume event)))]))

;; Generate a small relational language with explicit binding and observation
;; boundaries, including duplicated answers. x is its current input state.
(define (oracle term x)
  (match term
    ['zero '()]
    ['unit (list x)]
    [`(add ,n) (list (+ x n))]
    [`(delay ,a) (lambda () (oracle a x))]
    [`(or ,a ,b) (plus (oracle a x) (oracle b x))]
    [`(and ,a ,b) (bind (oracle a x) (lambda (y) (oracle b y)))]
    [`(once ,a)
     (let loop ([xs (oracle a x)])
       (cond [(null? xs) '()]
             [(pair? xs) (list (car xs))]
             [else (lambda () (loop (xs)))]))]
    [`(ifte ,a ,b ,c)
     (let loop ([xs (oracle a x)])
       (cond [(null? xs) (oracle c x)]
             [(pair? xs) (bind xs (lambda (y) (oracle b y)))]
             [else (lambda () (loop (xs)))]))]))

(define (interpret term x)
  (match term
    ['zero (search/thunk (lambda () (amb)))]
    ['unit (search/thunk (lambda () x))]
    [`(add ,n) (search/thunk (lambda () (+ x n)))]
    [`(delay ,a)
     (search-delay (lambda () (interpret a x)))]
    [`(or ,a ,b) (search-merge (interpret a x) (interpret b x))]
    [`(and ,a ,b)
     (search-bind (interpret a x) (lambda (y) (interpret b y)))]
    [`(once ,a) (search-once (interpret a x))]
    [`(ifte ,a ,b ,c)
     (search-ifte (interpret a x)
                  (lambda (y) (interpret b y))
                  (interpret c x))]))

;; Direct-style interpretation exercises captured choice trees and the local
;; boundary that separates a bind's source from its continuation.
(define (direct term x)
  (match term
    ['zero (amb)]
    ['unit x]
    [`(add ,n) (+ x n)]
    [`(delay ,a) (suspend) (direct a x)]
    [`(or ,a ,b) (amb (direct a x) (direct b x))]
    [`(and ,a ,b)
     (direct b (call-with-local-search (lambda () (direct a x))))]
    [`(once ,a) (call-with-local-search (lambda () (direct a x)) 1)]
    [`(ifte ,a ,b ,c)
     (define result
       (call-with-local-default (lambda () (direct a x)) (lambda () #f)))
     (if result (direct b result) (direct c x))]))

(define (generate depth bind?)
  (if (zero? depth)
      (list-ref '(zero unit (add 1) (add -1)) (random 4))
      (let ([sub (lambda () (generate (sub1 depth) bind?))])
        (case (random (if bind? 6 3))
          [(0) (generate 0 bind?)]
          [(1) `(delay ,(sub))]
          [(2) `(or ,(sub) ,(sub))]
          [(3) `(and ,(sub) ,(sub))]
          [(4) `(once ,(sub))]
          [(5) `(ifte ,(sub) ,(sub) ,(sub))]))))

(parameterize ([current-pseudo-random-generator
                (make-pseudo-random-generator)])
  (random-seed 20261106)
  (for ([i (in-range 2000)])
    (define term (generate 5 #t))
    (define expected (take-stream (oracle term 0) 10000))
    (check-equal? (collect (interpret term 0)) expected (format "~s" term))
    (check-equal? (search-trace (interpret term 0))
                  (oracle-trace (oracle term 0)) (format "trace ~s" term)))
  (for ([i (in-range 2000)])
    (define term (generate 6 #f))
    (check-equal? (run (lambda () (direct term 0)))
                  (take-stream (oracle term 0) 10000) (format "~s" term))
    (check-equal? (search-trace (search/thunk (lambda () (direct term 0))))
                  (oracle-trace (oracle term 0)) (format "trace ~s" term)))
  (for ([i (in-range 2000)])
    (define term (generate 5 #t))
    (check-equal? (run (lambda () (direct term 0)))
                  (take-stream (oracle term 0) 10000) (format "local ~s" term))
    (check-equal? (search-trace (search/thunk (lambda () (direct term 0))))
                  (oracle-trace (oracle term 0)) (format "local trace ~s" term))))

;; Infinite suspended branches must remain productive, with local grouping.
(define (repeat tag) (suspend) (amb tag (repeat tag)))
(define (repeat-stream tag)
  (lambda () (cons tag (repeat-stream tag))))
(define (prefix thunk)
  (for/list ([_ (in-range 12)] [x (search->sequence (search/thunk thunk))]) x))
(check-equal? (prefix (lambda () (amb (repeat 'a)
                                     (amb (repeat 'b) (repeat 'c)))))
              (take-stream (plus (repeat-stream 'a)
                                 (plus (repeat-stream 'b) (repeat-stream 'c)))
                           12))

;; A failed first answer in THEN must not cause IFTE to take OTHERWISE.
(check-equal? (collect (interpret '(ifte unit zero (add 99)) 0)) '())

;; A minimal counterexample to replacing bind with function composition.
;; The final suspension is inside each branch's captured continuation in
;; the direct version. An explicit bind retains the source's mature order.
(check-equal? (run (lambda ()
                    (define x (amb (amb 1 2) 3))
                    (suspend)
                    x)) '(3 1 2))
(check-equal? (collect
               (search-bind (search/thunk (lambda () (amb (amb 1 2) 3)))
                            (lambda (x)
                              (search/thunk (lambda () (suspend) x)))))
              '(1 2 3))
(check-equal? (run (lambda ()
                    (define x
                      (call-with-local-search (lambda () (amb (amb 1 2) 3))))
                    (suspend)
                    x)) '(1 2 3))

;; Dynamic bindings survive suspension and do not leak into siblings.
(define context (make-parameter 'outside))
(check-equal? (run (lambda ()
                    (amb (parameterize ([context 'inside])
                           (suspend) (context))
                         (context)))) '(outside inside))
(check-equal? (context) 'outside)
(check-exn #rx"branch error"
           (lambda () (run (lambda () (suspend) (error "branch error")))))

;; A perpetually suspended branch cannot prevent its sibling's first answer.
;; Keep a timeout so a scheduling regression fails instead of hanging CI.
(define found (box #f))
(define worker
  (thread
   (lambda ()
     (define (never) (suspend) (never))
     (set-box! found
               (collect (search-once
                         (search/thunk (lambda () (amb (never) 42)))))))))
(define completed? (sync/timeout 5 worker))
(unless completed? (kill-thread worker))
(check-not-false completed?)
(check-equal? (unbox found) '(42))

(define-values (more? get) (sequence-generate
                            (search->sequence (search/thunk (lambda () 7)))))
(check-true (more?))
(check-true (more?))
(check-equal? (get) 7)
(check-false (more?))
(check-false (more?))
