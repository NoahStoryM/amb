#lang racket/base

;; ===================================================================
;; Choice as merge: the effects of a search
;; ===================================================================
;;
;; A search folds a direct-style computation into a generator of events
;; (see private/generator.rkt): an answer, pauses, or the end.  Each
;; effect says what it contributes, in terms of the generator of the
;; rest of the computation, `k`:
;;
;;   return vs    the answer vs, and then the end
;;   amb-fail     the end
;;   amb-pause    one pause, and then k
;;   make-amb     for the alternatives a, b that it is supplied with,
;;                  (merge (λ () (k a)) (λ () (merge (λ () (k b)) end)))
;;                where merge is the merger of the choice
;;
;; `amb-generate*` runs a computation as a search of its own.

(require racket/control
         "private/generator.rkt"
         (only-in "merge.rkt" amb-append))

(provide make-amb
         amb-generate*
         amb-fail
         amb-pause
         current-amb-merger)

;; Every effect is delimited by the nearest search.
(define amb-prompt-tag (make-continuation-prompt-tag 'amb))

;; The merger of the choices that are made while it is the current one.
(define current-amb-merger
  (make-parameter
   amb-append
   (λ (merge)
     (unless (and (procedure? merge) (procedure-arity-includes? merge 2))
       (raise-argument-error 'current-amb-merger
                             "(procedure-arity-includes/c 2)"
                             merge))
     merge)
   'current-amb-merger))

;; (amb-generate* thk) -> (values Result Generator)
;;
;; Runs (thk) as a search of its own, up to its first event, as
;; `sequence-generate*` does for a sequence.  No effect of (thk) reaches
;; an enclosing search, and nothing more of the search runs until the
;; generator is called.
(define (amb-generate* thk)
  (unless (and (procedure? thk) (procedure-arity-includes? thk 0))
    (raise-argument-error 'amb-generate* "(-> any)" thk))
  (reset-at amb-prompt-tag
    (values (call-with-values thk list) end)))

;; A choice among the alternatives that the generator `next` supplies:
;; each answer of `next` is the values of one alternative, which
;; `make-amb` returns to its continuation.  The alternatives are merged
;; from the right, by the merger that is current when the choice is
;; made; the last one is merged with the end of the supply.
;;
;; The pauses of the supply are pauses of the choice, where they are.
(define (make-amb next)
  (unless (and (procedure? next) (procedure-arity-includes? next 0))
    (raise-argument-error 'make-amb "(-> any)" next))
  (define merge (current-amb-merger))
  (shift-at amb-prompt-tag k
    (let loop ([next next])
      (let-values ([(v next) (next)])
        (cond
          [(list? v) ((merge (λ () (apply k v)) (λ () (loop next))))]
          [(pause-count? v) (values v (λ () (loop next)))]
          [(not v) (values v next)]
          [else (raise-result-error 'make-amb v)])))))

;; The computation ends here, with no answer.
(define (amb-fail)
  (shift-at amb-prompt-tag k (end)))

;; The search pauses here: a merger may pass the turn on to another
;; alternative.  Returns (void) when the computation goes on.
(define (amb-pause)
  (shift-at amb-prompt-tag k (values 1 (λ () (k (void))))))
