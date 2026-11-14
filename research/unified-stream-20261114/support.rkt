#lang racket/base
(require racket/runtime-path racket/match)
(provide (all-defined-out))
(define-runtime-path here ".")
(struct api (name new? get) #:transparent)
(define (load-api name)
  (define path (build-path here (if (eq? name 'A) "baseline/kernel.rkt"
                                  (format "~a.rkt" (string-downcase (symbol->string name))))))
  (api name (memq name '(C D)) (λ (sym) (dynamic-require path sym))))
(define (op a sym) ((api-get a) sym))
(define (merge-of a m)
  (op a (string->symbol (format "amb-~a" m))))

;; One-time adapter for the baseline and B test drivers, not part of C/D.
;; It is outside timed regions when possible; benchmarks also have raw drains.
(define (old->values s)
  (λ ()
    (cond
      [(null? s) (values #f (λ () (error 'old->values "ended")))]
      [(pair? s) (values (car s) (λ () ((old->values ((cdr s))))))]
      [else (values 1 (λ () ((old->values (s)))))])))
(define (as-values a s) (if (api-new? a) s (old->values s)))
(define (stream-empty a) (if (api-new? a) (op a 'done) '()))
(define (unit a vs)
  (if (api-new? a) ((op a 'unit) vs) (cons vs (λ () '()))))
(define (raw-stream a es)
  (if (api-new? a)
      (λ ()
        (cond
          [(null? es) (values #f (op a 'no-more))]
          [(eq? (car es) 'pause) (values 1 (raw-stream a (cdr es)))]
          [else (values (car es) (raw-stream a (cdr es)))]))
      (cond
        [(null? es) '()]
        [(eq? (car es) 'pause) (λ () (raw-stream a (cdr es)))]
        [else (cons (car es) (λ () (raw-stream a (cdr es))))])))
(define (merge-raw a m s t)
  ((merge-of a m) (if (eq? (api-name a) 'A) s (if (api-new? a) s (λ () s))) t))

;; Every call has finite fuel, including observation of finite streams.
(define (observe a s fuel [snapshot (λ () #f)])
  (let loop ([s (as-values a s)] [n fuel] [log '()])
    (cond
      [(zero? n) (reverse log)]
      [else
       (define-values (r next) (s))
       (cond
         [(not r) (reverse (cons (list 'end (snapshot)) log))]
         [(list? r) (loop next (sub1 n) (cons (list r (snapshot)) log))]
         [else
          (define count (min n r))
          (define logs (for/fold ([log log]) ([_ (in-range count)])
                         (cons (list 'pause (snapshot)) log)))
          (if (= count n) (reverse logs) (loop next (- n count) logs))])])))
(define (events a s fuel) (map car (observe a s fuel)))

;; Same direct program for all four kernels; generated goals come from the
;; unchanged baseline model, whose explicit-CPS spec is an independent oracle.
(define (run-goal a g [state '(start)] [log! void])
  (define (run g state)
    (match g
      [`(tag ,x) (log! x) (cons x state)]
      ['succeed state]
      ['fail ((op a 'amb-fail))]
      ['pause ((op a 'amb-pause)) state]
      [`(seq ,g1 ,g2) (run g2 (run g1 state))]
      [`(alt ,g1 ,g2)
       ((op a 'amb*) ((op a 'current-amb-merge))
                     (vector (λ () (run g1 state)) (λ () (run g2 state))))]
      [`(alt ,m ,g1 ,g2)
       ((op a 'amb*) (merge-of a m)
                     (vector (λ () (run g1 state)) (λ () (run g2 state))))]
      [`(local ,g) ((op a 'amb-reflect) ((op a 'amb-reify) (λ () (run g state))))]
      [`(once ,g) ((op a 'amb-once) (λ () (run g state)))]
      [`(ifte ,g1 ,g2 ,g3)
       (define stream ((op a 'amb-reify) (λ () (run g1 state))))
       (cond
         [(api-new? a)
          (define-values (r next) ((op a 'amb-await) stream))
          (if r
              (run g2 ((op a 'amb-reflect) (λ () (values r next))))
              (run g3 state))]
         [else
          (define ready ((op a 'amb-await) stream))
          (if (null? ready) (run g3 state)
              (run g2 ((op a 'amb-reflect) ready)))])]))
  (run g state))
