#lang racket/base

;; ===================================================================
;; amb/base: the operations, and the search that interprets them
;; ===================================================================
;;
;; A direct-style nondeterministic computation is a tree that is revealed
;; one node at a time: it returns (a leaf), fails, pauses, or chooses.
;; An observer of a search sees a sequence of events over time: an answer,
;; or "not yet" (a pause).  So a search is a function from the tree to
;;
;;   StepStream = '()                         ; no more events
;;              | (cons vs (→ StepStream))    ; an answer, then (lazily) the rest
;;              | (→ StepStream)              ; a step (a pause), then the rest
;;
;; and every operation says what it contributes to that stream, in terms
;; of the stream `(k v)` of the rest of the computation:
;;
;;   return vs   ↦  the stream of the one answer vs
;;   fail        ↦  '()
;;   pause       ↦  a pause, then (k)
;;   choose a b  ↦  (merge (k a) (λ () (k b)))
;;
;; This module is exactly that: the four operations (return is a plain
;; return), and `amb-reify`, the search that folds a computation into its
;; stream.  Everything else is defined from these:
;;
;;   amb/base        this module; the stream itself, and observing it,
;;                   are in private/data/step-stream.rkt
;;   amb/merge       strategies of a choice between two alternatives;
;;                   they only see streams, and need nothing from here
;;   amb (main)      `amb`, `for/amb`, the default strategy, the
;;                   shuffler, local search (`amb-reflect`, ...), and
;;                   observing a stream (`in-amb`, ...)
;;   amb/nary        strategies that see all the alternatives of a choice
;;
;; and under private/data, what they are written with: the stream and
;; observing it (step-stream.rkt), and the lists that amb/merge appends in
;; constant time (catenable.rkt).

(require racket/control
         "private/data/step-stream.rkt")

(provide
 ;; the operations
 amb-reify amb-fail amb-pause
 amb-choose amb-choose-from
 ;; strategies that see all the alternatives of a choice
 amb-strategy amb-strategy? amb-strategy-proc)


;; ---- Effects ----

(define tag (make-continuation-prompt-tag 'amb))

;; The stream of the events of (thk), which runs as a search of its own:
;; no effect of (thk) reaches the enclosing search, and nothing runs after
;; an event until the stream is forced further.
(define (amb-reify thk)
  (reset-at tag (step-stream-unit (call-with-values thk list))))

;; In the three effects, `k` is the rest of the computation, up to the
;; search: (k v) is the stream of the events of the rest when the
;; operation returns v.

(define (amb-fail)
  (shift-at tag k empty-step-stream))

(define (amb-pause)
  (shift-at tag k (step-stream-delay (k (void)))))


;; ---- Choices ----
;;
;; A choice has alternatives, and a strategy says how their streams are
;; merged.  The strategy is one of
;;
;;   a merge       (merge s t): the stream s, and the stream of the thunk
;;                 t.  A choice among more than two alternatives is
;;                 merged from the right:
;;                   (merge (k a) (λ () (merge (k b) (λ () (k c)))))
;;   a strategy    (amb-strategy proc), which sees all the alternatives:
;;                 (proc next), where (next) is the stream of the next
;;                 alternative, or #f when there are no more.  Each
;;                 alternative starts to run when (next) is called.
;;
;; and the alternatives come in a vector (`amb-choose`) or from a
;; sequence (`amb-choose-from`).

(struct amb-strategy (proc))

;; A choice among the thunks of the mutable vector `alts`, which is used
;; up.  The chosen thunk is called where the choice was made, so a choice
;; made inside an alternative is a choice of the same search.
(define (amb-choose m alts)
  ((shift-at tag k (merge-vector m k alts))))

;; A slot is emptied when its thunk is taken, so that a choice does not
;; hold on to the alternatives it is done with.
(define (take! alts i)
  (define alt (vector-ref alts i))
  (unless alt
    (error 'amb "an alternative is taken twice; a stream is single-pass"))
  (vector-set! alts i #f)
  alt)

(define (merge-vector m k alts)
  (define n (vector-length alts))
  (cond
    [(amb-strategy? m)
     (define i 0)
     ((amb-strategy-proc m)
      (λ ()
        (and (< i n)
             (let ([alt (take! alts i)])
               (set! i (add1 i))
               (k alt)))))]
    [(zero? n) empty-step-stream]
    [else
     ;; the last alternative is merged with nothing
     (let loop ([i 0])
       (define s (k (take! alts i)))
       (if (= (add1 i) n)
           s
           (m s (step-stream-delay (loop (add1 i))))))]))

;; A choice among the elements of the sequence, which are generated as
;; the search goes on.  The sequence is not asked whether it has another
;; element before the turn passes on to it, so the end of the sequence is
;; merged as a stream of its own: a merge m is used as
;;   (m (k a) (λ () (m (k b) (λ () '()))))
;; which is the choice among a vector when s ∨ '() = s.
(define (amb-choose-from m seq)
  (define-values (more? get) (sequence-generate seq))
  (shift-at tag k
    (if (amb-strategy? m)
        ((amb-strategy-proc m) (λ () (and (more?) (k (get)))))
        (let loop ()
          (if (more?)
              (let ([v (get)]) (m (k v) loop))
              empty-step-stream)))))
