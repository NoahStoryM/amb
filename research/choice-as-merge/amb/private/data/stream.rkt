#lang racket/base

;; ===================================================================
;; The stream of the events of a search
;; ===================================================================
;;
;;   Stream = '()                     ; no more events
;;          | (cons vs (→ Stream))    ; an answer (a list of values),
;;                                    ; then (lazily) the rest
;;          | (→ Stream)              ; a pause, then the rest
;;
;; What it has to be (see README): pauses and answers are different
;; events, since a strategy that lets another alternative run at a pause
;; has to tell them apart; every step is lazy, the rest after an answer
;; too, so that an answer is seen before the computation goes on; and the
;; end is not an endless pause.  It is the coalgebra νX. 1 + V × X + X.
;; How it is written is chosen: a pause and a lazy rest are both thunks,
;; so that a merge is its equations, and a thunk is not memoized, so that
;; a stream may be forced inside a continuation that is resumed again.
;; A stream is therefore single-pass.

(provide done unit
         in-answers amb-take amb-events)

;; The stream of one answer.  Its rest is the thunk `done`, which
;; `amb-reflect` recognizes: a stream known to end after its first answer
;; (the case `(a)` of the first edition's streams).
(define (done) '())
(define (unit vs) (cons vs done))


;; ---- Observing a stream ----

;; The answers of a stream, as a single-pass sequence; pauses are skipped.
(define (in-answers s)
  (make-do-sequence
   (λ ()
     (define (more? _)
       (let loop ()
         (when (procedure? s) (set! s (s)) (loop)))
       (pair? s))
     (define (get _)
       (define vs (car s))
       (set! s (cdr s))
       (apply values vs))
     (values get void (void) more? #f #f))))

;; The first n answers (all of them when n is #f), one value each.
(define (amb-take n s)
  (for/list ([_ (in-range (or n +inf.0))]
             [v (in-answers s)])
    v))

;; The first `fuel` events: each answer (one value), or 'pause.  Nothing
;; is forced after the last of them.
(define (amb-events s [fuel +inf.0])
  (let loop ([s s] [fuel fuel])
    (cond
      [(or (null? s) (<= fuel 0)) '()]
      [(pair? s)
       (cons (caar s) (if (<= fuel 1) '() (loop ((cdr s)) (sub1 fuel))))]
      [else
       (cons 'pause (if (<= fuel 1) '() (loop (s) (sub1 fuel))))])))
