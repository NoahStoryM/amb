#lang racket/base

(require (for-syntax racket/base)
         racket/control
         racket/sequence
         racket/stream)

(provide in-amb/do
         make-amb amb for/amb for*/amb
         amb-fail amb-pause
         current-amb-merger amb-append amb-interleave amb-alternate amb-level)


;; ---- Generator protocol: list = values, #f = end, n = n pauses. ----
;; A raw 0 advances to next without producing an event or changing turns.

(define false? not)
(define natural? exact-nonnegative-integer?)

(define (raise-empty-sequence-error)
  (raise (exn:fail:contract
          "sequence has no more values"
          (current-continuation-marks))))
(define (end) (values #f raise-empty-sequence-error))

(define (pauses n g) (if (zero? n) g (λ () (values 1 (pauses (sub1 n) g)))))


;; ---- Merge: equations on generators. ----

(define ((amb-append s t))
  (let-values ([(v s) (s)])
    (cond [(eqv? v 0) ((amb-append s t))]
          [(or (list? v) (natural? v)) (values v (amb-append s t))]
          [(false? v) (t)]
          [else (raise-argument-error 'amb-append "(or/c list? natural? #f)" v)])))

(define ((amb-interleave s t))
  (let-values ([(v s) (s)])
    (cond [(list? v) (values v (amb-interleave s t))]
          [(eqv? v 0) ((amb-interleave s t))]
          [(natural? v) (values 1 (amb-interleave t (pauses (sub1 v) s)))]
          [(false? v) (t)]
          [else (raise-argument-error 'amb-interleave "(or/c list? natural? #f)" v)])))

(define ((amb-alternate s t))
  (let-values ([(v s) (s)])
    (cond [(list? v) (values v (amb-alternate t s))]
          [(eqv? v 0) ((amb-alternate s t))]
          [(natural? v) (values 1 (amb-alternate t (pauses (sub1 v) s)))]
          [(false? v) (t)]
          [else (raise-argument-error 'amb-alternate "(or/c list? natural? #f)" v)])))

(define ((amb-level s t))
  (let-values ([(v s) (s)])
    (cond
      [(list? v) (values v (amb-level s t))]
      [(eqv? v 0) ((amb-level s t))]
      [(natural? v)
       (let scan ([t t])
         (let-values ([(u t) (t)])
           (cond
             [(list? u) (values u (λ () (scan t)))]
             [(eqv? u 0) (scan t)]
             [(natural? u) (values 1 (amb-level (pauses (sub1 v) s) (pauses (sub1 u) t)))]
             [(false? u) (values 1 (pauses (sub1 v) s))]
             [else (raise-argument-error 'amb-level "(or/c list? natural? #f)" u)])))]
      [(false? v) (t)]
      [else (raise-argument-error 'amb-level "(or/c list? natural? #f)" v)])))

(define current-amb-merger (make-parameter amb-append))


;; ---- Choice: capture k once, then merge alternatives with that k. ----

(define amb-prompt-tag (make-continuation-prompt-tag 'amb))

(define (make-amb next)
  (define merge (current-amb-merger))
  (shift-at amb-prompt-tag k
    (let loop ([next next])
      (let-values ([(v next) (next)])
        (cond
          [(list? v) ((merge (λ () (apply k v)) (λ () (loop next))))]
          [(eqv? v 0) (loop next)]
          [(natural? v) (values v (λ () (loop next)))]
          [(false? v) (values v next)]
          [else (raise-argument-error 'make-amb "(or/c list? natural? #f)" v)])))))
(define (sequence->amb s) (make-amb (λ () (sequence-generate* s))))

(define (amb-fail)  (shift-at amb-prompt-tag k (values #f raise-empty-sequence-error)))
(define (amb-pause) (shift-at amb-prompt-tag k (values 1 (compose1 k void))))


;; ---- Single-pass sequence boundary; no replay or phase checks. ----

(define (in-amb*/do thk [handler #f])
  (define (gen) (reset-at amb-prompt-tag (values (call-with-values thk list) end)))
  (define cache #f)
  (define done? #f)
  (define (more? . _)
    (unless (or cache done?)
      (let loop ()
        (define-values (v next) (gen))
        (set! gen next)
        (cond
          [(list? v) (set! cache v)]
          [(false? v) (set! done? #t)]
          [(eqv? v 0) (loop)]
          [(natural? v)
           (cond
             [handler
              (set! gen (pauses (sub1 v) next))
              (set! cache (call-with-values handler list))]
             [else (loop)])]
          [else (raise-argument-error 'in-amb*/do "(or/c list? natural? #f)" v)])))
    (list? cache))
  (define (get . _)
    (if (more?)
        (let ([v* cache])
          (set! cache #f)
          (apply values v*))
        (raise-empty-sequence-error)))
  (make-do-sequence
   (λ ()
     (initiate-sequence
      #:init-pos           0
      #:continue-with-pos? more?
      #:pos->element       get
      #:next-pos           add1))))


;; ---- Derived syntax: call the chosen thunk in the captured continuation. ----

(define-syntax in-amb/do
  (syntax-rules ()
    [(_ e) (in-amb*/do (λ () e))]
    [(_ e #:on-pause handler) (in-amb*/do (λ () e) handler)]))

(define-syntax amb
  (syntax-rules ()
    [(_) (amb-fail)]
    [(_ e ...) ((sequence->amb (vector (λ () e) ...)))]))

(define-syntaxes (for/amb for*/amb)
  (let ()
    (define (make-for/amb for/amb for/stream)
      (with-syntax ([for/amb for/amb] [for/stream for/stream])
        (syntax-rules ()
          [(_ (clause ...) #:break guard body ...)
           (for/amb (clause ... #:break guard) body ...)]
          [(_ (clause ...) #:final guard body ...)
           (for/amb (clause ... #:final guard) body ...)]
          [(_ (clause ...) body ...)
           ((sequence->amb (for/stream (clause ...) (λ () body ...))))])))
    (values (make-for/amb #'for/amb  #'for/stream)
            (make-for/amb #'for*/amb #'for*/stream))))
