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
         "generator.rkt"
         (only-in "../merge.rkt" amb-append))

(provide make-amb
         amb-generate*
         amb-fail
         amb-pause
         current-amb-merger
         with-amb-mark
         call-with-amb-mark)

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

;; ---- The supply of alternatives ----
;;
;; The alternatives of a choice are supplied by code that is not part of
;; the search: the generator of `make-amb`, and with it the clauses of
;; `for/amb` and the sequence of `sequence->amb`.  That code runs when
;; the merger of the choice turns to the next alternative, wherever that
;; is; an effect in it has no meaning for the search it supplies, and is
;; an error.
;;
;; (with-amb-mark e) evaluates e as such code, and (call-with-amb-mark
;; thk) calls thk as such code: `make-amb` does it for its generator,
;; and a form that collects alternatives before it calls `make-amb` does
;; it for the code that collects them.  An effect looks for the mark
;; no further than the nearest search, so a search of its own that the
;; code runs, or goes on with, is not concerned: the body of a search
;; and the continuations that it captures run inside the boundary of
;; that search (the reset of `amb-generate*`, and the prompt of every
;; continuation that `shift-at` hands out).
;;
;; That is all the boundary covers.  The other code that runs when a
;; generator is called -- a supplier, a merger that is not one of
;; amb/merge -- runs in the dynamic environment of whoever calls the
;; generator: an effect there is not an effect of the search that the
;; generator belongs to.
(define supply-key (make-continuation-mark-key 'amb-supply))

(define-syntax-rule (with-amb-mark e)
  (with-continuation-mark supply-key #t e))

;; The same, for where a macro of this module cannot be used (Typed
;; Racket has no type for `supply-key`).
(define (call-with-amb-mark thk)
  (with-continuation-mark supply-key #t (thk)))

;; Called by every effect before anything else.
(define (check-effect who)
  (cond
    [(not (continuation-prompt-available? amb-prompt-tag))
     (raise
      (exn:fail:contract
       (if (continuation-mark-set-first #f supply-key #f)
           (supply-message who)
           (format "~a: not in a search" who))
       (current-continuation-marks)))]
    [(continuation-mark-set-first #f supply-key #f amb-prompt-tag)
     (raise (exn:fail:contract (supply-message who) (current-continuation-marks)))]))

(define (supply-message who)
  (format (string-append
           "~a: not allowed while the alternatives of a choice are supplied;\n"
           " the generator of make-amb, the clauses of for/amb and the shuffler\n"
           " must not choose, fail or pause in the search that they supply")
          who))


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
  (check-effect 'make-amb)
  (define merge (current-amb-merger))
  (shift-at amb-prompt-tag k
    (let loop ([next next])
      (let-values ([(v next) (with-amb-mark (next))])
        (cond
          [(list? v) ((merge (λ () (apply k v)) (λ () (loop next))))]
          [(pause-count? v) (values v (λ () (loop next)))]
          [(not v) (values v next)]
          [else (raise-result-error 'make-amb v)])))))

;; The computation ends here, with no answer.
(define (amb-fail)
  (check-effect 'amb-fail)
  (shift-at amb-prompt-tag k (end)))

;; The search pauses here: a merger may pass the turn on to another
;; alternative.  Returns (void) when the computation goes on.
(define (amb-pause)
  (check-effect 'amb-pause)
  (shift-at amb-prompt-tag k (values 1 (λ () (k (void))))))
