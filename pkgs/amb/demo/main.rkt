#lang racket/base

;; A small, independent choice-as-merge kernel for study and exposition.
;; Adapted from research/minimal-reference-20261114/demo.rkt (2613c43).
;; Supply effects and exception recovery are left to the full library.
;;
;; The kernel uses shift and reset of racket/control, to show the
;; equations of the events.  It is not a reference for where the full
;; library runs host code: there, an effect leaves its search before
;; the code that interprets a choice runs (shift0), and amb-fail does
;; not capture its continuation (see private/core.rkt).

(require (for-syntax racket/base)
         racket/control
         racket/sequence
         racket/stream)

(provide amb-generate* in-amb in-amb* in-amb/do in-amb*/do
         make-amb sequence->amb amb for/amb for*/amb
         amb-fail amb-pause
         current-amb-merger amb-append amb-interleave amb-alternate amb-level)


;; ---- Generator protocol: list = values, #f = end, n = n pauses. ----
;; Generator = () -> (values (or/c list? exact-positive-integer? #f) Generator)
;; An empty list is a zero-valued answer. After #f, do not call next.

(define false? not)

(define (raise-empty-sequence-error)
  (raise (exn:fail:contract
          "sequence has no more values"
          (current-continuation-marks))))
(define (end) (values #f raise-empty-sequence-error))

(define (pauses n g) (if (zero? n) g (λ () (values 1 (pauses (sub1 n) g)))))


;; ---- Merge: equations on generators. ----

(define ((amb-append s t))
  (let-values ([(v s) (s)])
    (cond
      [(or (list? v) (exact-positive-integer? v)) (values v (amb-append s t))]
      [(false? v) (t)]
      [else (raise-argument-error 'amb-append "(or/c list? exact-positive-integer? #f)" v)])))

(define ((amb-interleave s t))
  (let-values ([(v s) (s)])
    (cond
      [(list? v) (values v (amb-interleave s t))]
      [(exact-positive-integer? v) (values 1 (amb-interleave t (pauses (sub1 v) s)))]
      [(false? v) (t)]
      [else (raise-argument-error 'amb-interleave "(or/c list? exact-positive-integer? #f)" v)])))

(define ((amb-alternate s t))
  (let-values ([(v s) (s)])
    (cond
      [(list? v) (values v (amb-alternate t s))]
      [(exact-positive-integer? v) (values 1 (amb-alternate t (pauses (sub1 v) s)))]
      [(false? v) (t)]
      [else (raise-argument-error 'amb-alternate "(or/c list? exact-positive-integer? #f)" v)])))

(define ((amb-level s t))
  (let-values ([(v s) (s)])
    (cond
      [(list? v) (values v (amb-level s t))]
      [(exact-positive-integer? v)
       (let scan ([t t])
         (let-values ([(u t) (t)])
           (cond
             [(list? u) (values u (λ () (scan t)))]
             [(exact-positive-integer? u) (values 1 (amb-level (pauses (sub1 v) s) (pauses (sub1 u) t)))]
             [(false? u) (values 1 (pauses (sub1 v) s))]
             [else (raise-argument-error 'amb-level "(or/c list? exact-positive-integer? #f)" u)])))]
      [(false? v) (t)]
      [else (raise-argument-error 'amb-level "(or/c list? exact-positive-integer? #f)" v)])))

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
          [(exact-positive-integer? v) (values v (λ () (loop next)))]
          [(false? v) (values v next)]
          [else (raise-argument-error 'make-amb "(or/c list? exact-positive-integer? #f)" v)])))))
(define (sequence->amb s) (make-amb (λ () (sequence-generate* s))))

(define (amb-fail)  (shift-at amb-prompt-tag k (values #f raise-empty-sequence-error)))
(define (amb-pause) (shift-at amb-prompt-tag k (values 1 (compose1 k void))))


;; ---- Sequence boundaries: one cursor, optionally cached as a stream. ----

(define (amb-generate* thk)
  (reset-at amb-prompt-tag
    (values (call-with-values thk list) end)))

(define (in-amb*/do thk [handler #f])
  (define (gen) (amb-generate* thk))
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
          [(exact-positive-integer? v)
           (cond
             [handler
              (set! gen (pauses (sub1 v) next))
              (set! cache (call-with-values handler list))]
             [else (loop)])]
          [else (raise-argument-error 'in-amb*/do "(or/c list? exact-positive-integer? #f)" v)])))
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


(define (in-amb* thk [handler #f])
  (sequence->stream (in-amb*/do thk handler)))


;; ---- Derived syntax: call the chosen thunk in the captured continuation. ----

(define-syntax in-amb
  (syntax-rules ()
    [(_ e) (in-amb* (λ () e))]
    [(_ e #:on-pause handler) (in-amb* (λ () e) handler)]))

(define-syntax in-amb/do
  (syntax-rules ()
    [(_ e) (in-amb*/do (λ () e))]
    [(_ e #:on-pause handler) (in-amb*/do (λ () e) handler)]))

(define-syntax-rule (amb e ...)
  ((sequence->amb (vector (λ () e) ...))))

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
