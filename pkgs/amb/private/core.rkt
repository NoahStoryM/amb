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
;;
;; ---- Control ----
;;
;; A search is a prompt of `amb-prompt-tag`.  An effect captures its
;; continuation up to the nearest one, k, aborts to it, and then runs
;; what it contributes outside it: with shift0, not shift.  When k is
;; resumed, it runs in a prompt of its own, so that its next effect is
;; handled by it.  Every prompt of the tag is installed by `in-search`,
;; whose handler calls the thunk of the abort without installing the
;; prompt again; that is what puts the code of an effect outside the
;; search.  The prompts are installed nowhere else.

(require "tag.rkt"
         "generator.rkt"
         (only-in "../merge.rkt" amb-append))

(provide make-amb
         empty-choice
         amb-generate*
         amb-fail
         amb-pause
         current-amb-merger
         with-amb-mark
         call-with-amb-mark)

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
;; that search (the prompt of `amb-generate*`, and the prompt in which
;; every captured continuation is resumed).
;;
;; That is all the boundary covers.  The code that interprets a choice
;; -- the supply of `make-amb`, and the merger, from its first call on
;; -- runs outside the search that it serves, in the dynamic environment
;; of whoever calls it: the first time, the caller of the search, after
;; the effect left it; later, whoever asks for the next event.  An
;; effect there is not an effect of the search that the generator
;; belongs to.
(define-syntax-rule (with-amb-mark e)
  (call-with-amb-mark (λ () e)))

;; The mark belongs to this call; the syntax above only supplies a thunk.
(define (call-with-amb-mark thk)
  (with-continuation-mark amb-mark-key #t (thk)))

;; Called by every effect before anything else.
(define (check-effect who)
  (cond
    [(not (continuation-prompt-available? amb-prompt-tag))
     (raise
      (exn:fail:contract
       (if (continuation-mark-set-first #f amb-mark-key #f)
           (supply-message who)
           (format "~a: not in a search" who))
       (current-continuation-marks)))]
    [(continuation-mark-set-first #f amb-mark-key #f amb-prompt-tag)
     (raise (exn:fail:contract (supply-message who) (current-continuation-marks)))]))

(define (supply-message who)
  (format (string-append
           "~a: not allowed while the alternatives of a choice are supplied;\n"
           " code that supplies alternatives must not choose, fail or pause in its search;\n"
           " for example, the generator of make-amb, a clause of for/amb, or the shuffler")
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
  (in-search (λ () (values (call-with-values thk list) end))))

;; The names of racket/control for the primitives of racket/base.
(define call/prompt call-with-continuation-prompt)
(define call/comp call-with-composable-continuation)
(define abort/cc abort-current-continuation)

;; (let/comp k body ...) binds k to the composable continuation up to
;; the nearest search, as let/cc does for call/cc.
(define-syntax-rule (let/comp k body ...)
  (call/comp (λ (k) body ...) amb-prompt-tag))

;; (in-search thk) calls (thk) in a prompt of a search.  An abort to it
;; removes it, and calls the thunk of the abort in its place.
(define (in-search thk)
  (call/prompt thk amb-prompt-tag call-thunk))

(define (call-thunk thk) (thk))

;; (leave-search proc) captures the continuation k of the current
;; effect up to the nearest search, leaves that search, and calls
;; (proc k) there.  k is the bare composable continuation: the caller
;; must resume it with `resume`, or inside `in-search`, so that the
;; computation goes on in a search of its own.
(define (leave-search proc)
  (let/comp k
    (abort/cc amb-prompt-tag (λ () (proc k)))))

;; (resume k vs) resumes k with the values vs, in a search of its own.
(define (resume k vs)
  (in-search (if (and (pair? vs) (null? (cdr vs)))
                 (let ([v (car vs)]) (λ () (k v)))
                 (λ () (apply k vs)))))

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
  (leave-search
   (λ (k)
     (let loop ([next next])
       (let-values ([(v next) (with-amb-mark (next))])
         (cond
           [(list? v) ((merge (λ () (resume k v)) (λ () (loop next))))]
           [(pause-count? v) (values v (λ () (loop next)))]
           [(not v) (values v next)]
           [else (raise-result-error 'make-amb v)]))))))

;; Capture at the choice point even when there are no alternatives.
;; A continuation barrier still stops the choice.
(define (empty-choice)
  (check-effect 'make-amb)
  (let/comp _
    (abort/cc amb-prompt-tag end)))

;; The computation ends here, with no answer.  Nothing of it is resumed,
;; so its continuation is not captured: a continuation barrier between
;; here and the search does not stop it.
(define (amb-fail)
  (check-effect 'amb-fail)
  (abort/cc amb-prompt-tag end))

;; The search pauses here: a merger may pass the turn on to another
;; alternative.  Returns (void) when the computation goes on.
(define (amb-pause)
  (check-effect 'amb-pause)
  (leave-search (λ (k) (values 1 (λ () (in-search (λ () (k (void)))))))))
