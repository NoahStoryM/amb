;; Included separately by C and D: each has its own prompt and record types.
;; Stream = () -> (values (or/c list? #f positive-integer?) Stream)
;; A Stream and all its residual scheduling states are consumed once.

(define (no-more) (error 'amb-stream "stream already ended"))
(define (done) (values #f no-more))
(define (unit vs) (λ () (values vs done))) ; boundary convenience only

(define tag (make-continuation-prompt-tag 'amb-values))

;; The continuation and the reset return two values directly: no unit closure.
(define (amb-reify thk)
  (λ () (reset-at tag (values (call-with-values thk list) done))))

;; Optional migration boundary: start now, retain the first event in a closure.
(define (amb-reify/eager thk)
  (define-values (r next) ((amb-reify thk)))
  (λ () (values r next)))

(define (amb-fail) (shift-at tag k (done)))
(define (amb-pause)
  (shift-at tag k (values 1 (λ () (k (void))))))

(struct amb-strategy (proc))
(define current-amb-shuffler (make-parameter void))

(define (amb* m alts)
  (when (> (vector-length alts) 1) ((current-amb-shuffler) alts))
  (choose m alts))

(define (choose m alts)
  ((shift-at tag k (merge-vector m k alts))))

(define (take! alts i)
  (define alt (vector-ref alts i))
  (unless alt (error 'amb "an alternative is taken twice; a stream is single-pass"))
  (vector-set! alts i #f)
  alt)

(define (merge-vector m k alts)
  (define n (vector-length alts))
  (cond
    [(amb-strategy? m)
     (define i 0)
     (((amb-strategy-proc m)
       (λ ()
         (and (< i n)
              (let ([j i])
                (set! i (add1 i))
                (λ () (k (take! alts j))))))))]
    [(zero? n) (done)]
    [else
     (let loop ([i 0])
       (if (= (add1 i) n)
           (k (take! alts i))
           ((m (λ () (k (take! alts i)))
               (λ () (loop (add1 i)))))))]))

(define (sequence->amb-stream seq)
  (λ () (sequence-generate* seq)))

;; Multi-valued elements are preserved, including zero values.
(define (amb-from seq [m (current-amb-merge)])
  (define start (sequence->amb-stream seq))
  (shift-at tag k
    (cond
      [(amb-strategy? m)
       (define pending start)
       (((amb-strategy-proc m)
         (λ ()
           (and pending
                (let-values ([(r next) (pending)])
                  (set! pending (and r next))
                  (and r (λ () (apply k r))))))))]
      [else
       (let loop ([s start])
         (define-values (r next) (s))
         (if r ((m (λ () (apply k r)) (λ () (loop next)))) (done)))])))

(define (amb-interleave s t)
  (λ ()
    (define-values (r next) (s))
    (cond
      [(not r) (t)]
      [(list? r) (values r (amb-interleave next t))]
      [else (values 1 (amb-interleave t (after-pause r next)))])))

(define (amb-alternate s t)
  (λ ()
    (define-values (r next) (s))
    (cond
      [(not r) (t)]
      [(list? r) (values r (amb-alternate t next))]
      [else (values 1 (amb-alternate t (after-pause r next)))])))

;; State is a stream computation, not an already visible pause.
(struct appended (s ts)
  #:property prop:procedure
  (λ (self) (append-run (appended-s self) (appended-ts self))))

(define (amb-append s t) (appended s (cat-of t)))

(define (append-run s ts)
  (cond
    [(appended? s)
     (append-run (appended-s s) (cat-append (appended-ts s) ts))]
    [else
     (define-values (r next) (s))
     (cond
       [r (values r (if (null? ts) next (appended next ts)))]
       [(null? ts) (done)]
       [else (append-run (cat-first ts) (cat-rest ts))])]))

;; ws: continuations of streams which paused in this round, in order.
;; s: the current computation. ts: computations not yet run this round.
(struct leveled (ws s ts)
  #:property prop:procedure
  (λ (self) (level-run (leveled-ws self) (leveled-s self) (leveled-ts self))))

(define (amb-level s t) (leveled '() s (cat-of t)))

(define (level-run ws s ts)
  (cond
    [(leveled? s)
     (level-run (cat-append ws (leveled-ws s))
                (leveled-s s) (cat-append (leveled-ts s) ts))]
    [else
     (define-values (r next) (s))
     (cond
       [(list? r) (values r (leveled ws next ts))]
       [(not r) (level-next ws ts)]
       [else (level-next (cat-append ws (cat-of (after-pause r next))) ts)])]))

(define (level-next ws ts)
  (cond
    [(not (null? ts)) (level-run ws (cat-first ts) (cat-rest ts))]
    [(null? ws) (done)]
    [else (finish-round ws)]))

(define current-amb-merge (make-parameter amb-append))

;; Return two values, the first non-pause result and its next Stream.
;; This is a deliberate migration from the old ready-node amb-await API.
(define (amb-await s)
  (define-values (r next) (s))
  (cond
    [(or (not r) (list? r)) (values r next)]
    [else
     (for ([_ (in-range r)]) (amb-pause))
     (amb-await next)]))

(define (amb-reflect s [merge (current-amb-merge)])
  (define-values (r next) (amb-await s))
  (cond
    [(not r) (amb-fail)]
    [(eq? next done) (apply values r)]
    [else
     (choose merge
             (vector (λ () (apply values r))
                     (λ () (amb-reflect next merge))))]))

(define-syntax-rule (amb-local e) (amb-reflect (amb-reify (λ () e))))

(define (amb-once thk)
  (define-values (r next) (amb-await (amb-reify thk)))
  (if r (apply values r) (amb-fail)))

;; Guard the sequence object against re-initiation, as it owns the Stream.
;; Keep preparation in more?, not next-pos: for may advance a position
;; before delivering its element. No per-element pair is needed here.
;; Never call next when result is #f, and never prefetch past the last result.
(define (stream->sequence s events?)
  (define started? #f)
  (make-do-sequence
   (λ ()
     (when started? (error 'in-answers "single-pass sequence already initiated"))
     (set! started? #t)
     (define prepared? #f)
     (define finished? #f)
     (define result #f)
     (define (more? _)
       (unless (or prepared? finished?)
         (let loop ()
           (define-values (r next) (s))
           (cond
             [(not r) (set! finished? #t)]
             [(list? r)
              (set! result r) (set! s next) (set! prepared? #t)]
             [events?
              (set! result 'pause) (set! s (after-pause r next))
              (set! prepared? #t)]
             [else (set! s next) (loop)])))
       (not finished?))
     (define (get _)
       (set! prepared? #f)
       (if events? result (apply values result)))
     (values get void (void) more? #f #f))))

(define (in-answers s) (stream->sequence s #f))
(define (in-amb-events s) (stream->sequence s #t))
(define-syntax-rule (in-amb e) (in-answers (amb-reify (λ () e))))

(define (amb-take n s)
  (if (eqv? n 0) '()
      (for/list ([_ (in-range (or n +inf.0))] [v (in-answers s)]) v)))

;; Diagnostic list API retains the original one-value presentation.
;; Use in-amb-events to preserve zero/multiple values as a list event.
(define (amb-events s [fuel +inf.0])
  (let loop ([s s] [fuel fuel])
    (cond
      [(<= fuel 0) '()]
      [else
       (define-values (r next) (s))
       (cond
         [(not r) '()]
         [(list? r)
          (cons (car r) (if (<= fuel 1) '() (loop next (sub1 fuel))))]
         [else
          (cons 'pause (if (<= fuel 1) '()
                           (loop (after-pause r next) (sub1 fuel))))])])))
