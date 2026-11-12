#lang racket/base

;; The strategies of the kernel against their equations (the merges of
;; model.rkt), as functions on streams.  amb-append and amb-level are
;; flat in the kernel; this is where their records are flattened, kept
;; apart from the closures of the other two strategies, and merged again
;; after some of their events have been taken.
;;
;;   e ::= (stream id event ...)   the events: an answer, or pause
;;       | (m id e e)              the merge m of e and, as a thunk, e
;;       | (drop n e)              e after its first n events
;;
;; Every thunk records that it runs, so an observation of an expression
;; shows the events and also when each part of each stream ran: a merge
;; must not run a stream before its turn.

(require racket/match
         rackunit
         "../model.rkt")

;; The list of (cons event log): each event of e, with the thunks that
;; had run when it was observed, the most recent first; the first
;; element is (cons 'start log), for what the merges ran at once.
(define (observe e merge-of)
  (define log '())
  (define (log! . what) (set! log (cons what log)))
  (define (stream id es i)
    (define (rest) (log! id i) (stream id (cdr es) (add1 i)))
    (cond
      [(null? es) '()]
      [(eq? (car es) 'pause) rest]
      [else (cons (list (car es)) rest)]))
  (define (next s)                      ; s after its first event
    (cond
      [(null? s) s]
      [(pair? s) ((cdr s))]
      [else (s)]))
  (define (ev e)
    (match e
      [`(stream ,id ,es ...) (stream id es 0)]
      [`(drop ,n ,e)
       (for/fold ([s (ev e)]) ([_ n]) (next s))]
      [`(,m ,id ,e1 ,e2)
       ((merge-of m) (ev e1) (λ () (log! id) (ev e2)))]))
  (let loop ([s (ev e)] [event 'start])
    (cons (cons event log)
          (cond
            [(null? s) '()]
            [(pair? s) (loop ((cdr s)) (caar s))]
            [else (loop (s) 'pause)]))))

(define (same? e)
  (equal? (observe e reference-merge) (observe e kernel-merge)))

;; An expression with `size` streams; `ms` are the strategies to use.
(define (random-expression size ms)
  (define id 0)
  (define (fresh) (set! id (add1 id)) id)
  (let gen ([size size])
    (cond
      [(<= size 1)
       (define id (fresh))
       `(stream ,id
                ,@(for/list ([i (random 5)])
                    (if (zero? (random 2)) 'pause (list id i))))]
      [(zero? (random 6)) `(drop ,(random 4) ,(gen size))]
      [else
       ;; mostly to the left: that is where a flat strategy has work to do
       (define l (if (zero? (random 3))
                     (add1 (random (sub1 size)))
                     (sub1 size)))
       `(,(list-ref ms (random (length ms))) ,(fresh) ,(gen l) ,(gen (- size l)))])))

(random-seed 20261112)

(for ([ms '((append) (level) (append level) (append interleave alternate level))])
  (test-case (format "the kernel's merges are their equations: ~a" ms)
    (for ([_ 5000])
      (define e (random-expression (add1 (random 12)) ms))
      (check-true (same? e) (format "~s" e)))))

(test-case "what the observation sees"
  ;; a merge that runs the second stream too early is not the same,
  ;; even though the events are
  (define (eager-append s t)
    (let ([t* (t)])
      ((reference-merge 'append) s (λ () t*))))
  (define e '(append 0 (stream 1 a pause b) (stream 2 c)))
  (define (events o) (map car o))
  (define reference (observe e reference-merge))
  (define eager (observe e (λ (m) eager-append)))
  (check-equal? (events reference) '(start a pause b c))
  (check-equal? (events eager) (events reference))
  (check-not-equal? eager reference))
