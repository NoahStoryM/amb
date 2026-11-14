#lang racket/base

(require racket/control racket/stream)
(provide in-amb make-amb amb for/amb amb-fail amb-pause current-amb-merge
         amb-append amb-interleave amb-alternate amb-level)

;; Generator = () -> (values (or/c list? #f exact-positive-integer?) Generator)
;; '() is a zero-valued element. After #f, never call next. Single pass.
(define (end)
  (values #f (λ () (error 'amb "advanced after termination"))))

;; P^0(g) = g; every newly constructed pause is one logical event.
(define (pauses n g)
  (if (zero? n) g (λ () (values 1 (pauses (sub1 n) g)))))

;; ---- Merge: equations on generators, independent of search effects. ----

(define (amb-append s t)
  (λ ()
    (define-values (r next) (s))
    (if r (values r (amb-append next t)) (t))))

(define (amb-interleave s t)
  (λ ()
    (define-values (r next) (s))
    (cond
      [(not r) (t)]
      [(list? r) (values r (amb-interleave next t))]
      [else (values 1 (amb-interleave t (pauses (sub1 r) next)))])))

(define (amb-alternate s t)
  (λ ()
    (define-values (r next) (s))
    (cond
      [(not r) (t)]
      [(list? r) (values r (amb-alternate t next))]
      [else (values 1 (amb-alternate t (pauses (sub1 r) next)))])))

(define (amb-level s t)
  (λ ()
    (define-values (r next) (s))
    (cond
      [(not r) (t)]
      [(list? r) (values r (amb-level next t))]
      [else
       ;; Left has paused. Finish the right-hand round before resuming it.
       (let scan ([t t])
         (define-values (q more) (t))
         (cond
           [(not q) (values 1 (pauses (sub1 r) next))]
           [(list? q) (values q (λ () (scan more)))]
           [else (values 1 (amb-level (pauses (sub1 r) next)
                                     (pauses (sub1 q) more)))]))])))

;; ---- Choice: capture k once; put that same k in each alternative. ----

(define tag (make-continuation-prompt-tag 'amb-reference))
(define current-amb-merge (make-parameter amb-append))
(define phase (gensym 'amb-phase))

;; Supply/observation cannot capture the iteration machinery as a branch.
;; An independent in-amb resets this marker inside its own search boundary.
(define (check-effect who)
  (define where (continuation-mark-set-first #f phase))
  (when where
    (error who "search effect is not allowed while ~a" where)))

(define (make-amb g)
  (check-effect 'make-amb)
  (define merge (current-amb-merge))
  (shift-at tag k
    (let loop ([g g])
      (define-values (r next)
        (with-continuation-mark phase "supplying alternatives (make-amb / for/amb clauses)"
          (g)))
      (cond
        [(not r) (end)]
        [(list? r) ((merge (λ () (apply k r)) (λ () (loop next))))]
        [(exact-positive-integer? r) (values r (λ () (loop next)))]
        [else (raise-argument-error 'make-amb
                                    "(or/c list? #f exact-positive-integer?)" r)]))))

(define (amb-fail)
  (check-effect 'amb-fail)
  (shift-at tag k (end)))

(define (amb-pause)
  (check-effect 'amb-pause)
  (shift-at tag k (values 1 (λ () (k (void))))))

;; ---- Sequence boundary: lazy search, then ordinary Racket iteration. ----

(define (search-sequence thk on-pause)
  (define g
    (λ () (reset-at tag
            (with-continuation-mark phase #f
              (values (call-with-values thk list) end)))))
  (define started? #f)
  (make-do-sequence
   (λ ()
     (when started? (error 'in-amb "single-pass sequence already initiated"))
     (set! started? #t)
     ;; #t = not prepared, list = ready values, #f = ended.
     (define ready #t)
     (define (more? _)
       (when (eq? ready #t)
         (let pull ()
           (define-values (r next) (g))
           (set! g next)
           (cond
             [(or (not r) (list? r)) (set! ready r)]
             [on-pause
              (set! g (pauses (sub1 r) next))
              (set! ready
                    (with-continuation-mark phase "observing a pause"
                      (call-with-values on-pause list)))]
             [else (pull)])))
       (list? ready))
     (define (get _)
       (define vs ready)
       (set! ready #t)
       (apply values vs))
     ;; next-pos never advances the search; only an actual more? request does.
     (values get void (void) more? #f #f))))

;; ---- Derived syntax: the call of the selected thunk belongs to k. ----

(define-syntax in-amb
  (syntax-rules ()
    [(_ e) (search-sequence (λ () e) #f)]
    [(_ e #:on-pause handler) (search-sequence (λ () e) handler)]))

(define-syntax amb
  (syntax-rules ()
    [(_) (amb-fail)]
    [(_ e ...) ((make-amb (λ () (sequence-generate* (list (λ () e) ...)))))]))

(define-syntax for/amb
  (syntax-rules ()
    [(_ (clause ...) #:break guard body ...)
     (for/amb (clause ... #:break guard) body ...)]
    [(_ (clause ...) #:final guard body ...)
     (for/amb (clause ... #:final guard) body ...)]
    [(_ (clause ...) body ...)
     ((make-amb
       (λ () (sequence-generate* (for/stream (clause ...) (λ () body ...))))))]))
