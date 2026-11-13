#lang racket/base

;; ===================================================================
;; Step streams: the events of a search
;; ===================================================================
;;
;; A step stream is a lazy list of events, and an event is an answer or a
;; step:
;;
;;   StepStream = '()                         the end
;;              | (cons vs (→ StepStream))    an answer (a list of values),
;;                                            then (lazily) the rest
;;              | (→ StepStream)              a step, then the rest
;;
;; Up to isomorphism it is a stream of `Maybe`: a step is a `Nothing`,
;; an answer a `Just`.  As a coalgebra it is νX. 1 + V × X + X: a list
;; with silent steps, the `later` of the delay monad.  A step is not an
;; answer, and is erased when the answers are seen as a set; it is the
;; unit of time that a strategy uses to let another alternative run.
;;
;; What it has to be (see README): steps and answers are different events,
;; since a strategy that passes the turn on at a step has to tell them
;; apart; every part is lazy, the rest after an answer too, so that an
;; answer is seen before the computation goes on; and the end is not an
;; endless sequence of steps.
;;
;; How it is written is chosen: a step and the rest after an answer are
;; both suspensions, thunks that return a step stream, so that a merge is
;; its equations; and a suspension is not memoized, so that a stream may
;; be forced inside a continuation that is resumed again.  A step stream
;; is therefore single-pass.
;;
;; Code outside this module uses the operations below, and does not take
;; the representation apart itself.

(require (for-syntax racket/base))

(provide
 ;; building
 empty-step-stream step-stream-cons step-stream-delay
 step-stream-unit step-stream-done
 ;; taking apart
 step-stream-case step-stream-force
 ;; observing
 in-step-stream-answers step-stream-take step-stream-events)


;; ---- Building ----

(define empty-step-stream '())

;; An answer `vs` (a list of values), then the stream of the suspension
;; `rest`.
(define-syntax-rule (step-stream-cons vs rest) (cons vs rest))

;; A suspension: the stream of `e`, which is evaluated when the suspension
;; is forced, and every time it is.  As a stream, a suspension is a step.
(define-syntax-rule (step-stream-delay e ...) (λ () e ...))

;; The suspension of the empty stream.  `step-stream-unit` uses it as the
;; rest, so that a stream known to end after its first answer can be
;; recognized: `amb-reflect` does (the case `(a)` of the first edition's
;; streams).
(define (step-stream-done) empty-step-stream)

;; The stream of the one answer vs.
(define (step-stream-unit vs) (step-stream-cons vs step-stream-done))


;; ---- Taking apart ----

;; (step-stream-case s
;;   [() e ...]                 ; the end
;;   [(answer vs rest) e ...]   ; an answer, and the suspension of the rest
;;   [(step next) e ...])       ; a step: next is the suspension, s itself
(define-syntax (step-stream-case stx)
  ;; `answer` and `step` are recognized by name, whatever they are bound to
  (syntax-case stx ()
    [(_ s
        [() e0 ...]
        [(answer vs rest) e1 ...]
        [(step next) e2 ...])
     (and (eq? (syntax-e #'answer) 'answer) (eq? (syntax-e #'step) 'step)
          (identifier? #'vs) (identifier? #'rest) (identifier? #'next))
     #'(let ([x s])
         (cond
           [(null? x) e0 ...]
           [(pair? x) (let ([vs (car x)] [rest (cdr x)]) e1 ...)]
           [else (let ([next x]) e2 ...)]))]))

;; The stream of the suspension t.
(define-syntax-rule (step-stream-force t) (t))


;; ---- Observing ----

;; The answers of a stream, as a single-pass sequence; steps are skipped.
(define (in-step-stream-answers s)
  (make-do-sequence
   (λ ()
     (define (more? _)
       (let loop ()
         (step-stream-case s
           [() #f]
           [(answer vs rest) #t]
           [(step next) (set! s (step-stream-force next)) (loop)])))
     (define (get _)
       (step-stream-case s
         [() (error 'in-step-stream-answers "no more answers")]
         ;; the rest is not forced until more? is asked: as a stream, the
         ;; suspension is a step
         [(answer vs rest) (set! s rest) (apply values vs)]
         [(step next) (error 'in-step-stream-answers "not at an answer")]))
     (values get void (void) more? #f #f))))

;; The first n answers (all of them when n is #f), one value each.
(define (step-stream-take n s)
  (for/list ([_ (in-range (or n +inf.0))]
             [v (in-step-stream-answers s)])
    v))

;; The first `fuel` events: each answer (its first value), or 'pause for
;; a step.  Nothing is forced after the last of them.
(define (step-stream-events s [fuel +inf.0])
  (let loop ([s s] [fuel fuel])
    (if (<= fuel 0)
        '()
        (step-stream-case s
          [() '()]
          [(answer vs rest)
           (cons (car vs)
                 (if (<= fuel 1) '() (loop (step-stream-force rest) (sub1 fuel))))]
          [(step next)
           (cons 'pause
                 (if (<= fuel 1) '() (loop (step-stream-force next) (sub1 fuel))))]))))
