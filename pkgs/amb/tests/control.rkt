#lang racket/base

;; The control of a search (private/core.rkt): what an effect does to
;; the dynamic environment, where the code that interprets a choice
;; runs, and continuation barriers.
;;
;; An effect captures its continuation up to the search, leaves the
;; search, and runs its own code outside it (shift0); a resumed
;; continuation runs in a search of its own.  So:
;;
;;   - dynamic-wind, parameterize, continuation marks and break
;;     parameterizations are left by an effect and entered again when
;;     the computation is resumed;
;;   - the supply of a choice, and the merger from its first call on,
;;     run outside the search: there, the nearest prompt is that of an
;;     enclosing search, if any;
;;   - amb-fail does not capture its continuation, so a continuation
;;     barrier does not stop it; a choice and a pause, which capture
;;     theirs, are stopped by one.  A choice among no alternatives,
;;     (amb) or an empty for/amb, is a choice too.
;;
;; The cases are those of research/control-primitives-20261116 (branch
;; of the same name); the results that differ from v3 before this
;; change are the prompt in a supply at the top, the merger that
;; pauses, and amb-fail under a barrier.

(require rackunit
         amb amb/merge
         (only-in "../private/tag.rkt" amb-prompt-tag))

(define (end) (values #f (λ () (error "ended"))))
(define (first-line s) (car (regexp-split #rx"\n" s)))

;; the events of a search: P for a pause, the list of values of an answer
(define (events thk)
  (let loop ([gen (λ () (amb-generate* thk))])
    (define-values (r next) (gen))
    (cond [(not r) '()]
          [(list? r) (cons r (loop next))]
          [else (cons 'P (loop next))])))

(define log '())
(define (log! x) (set! log (cons x log)))
;; the result of thk, or the first line of its error, and what it logged
(define (with-log thk)
  (set! log '())
  (define r (with-handlers ([(λ (_) #t) (λ (e) (list 'error (if (exn? e) (first-line (exn-message e)) e)))]) (thk)))
  (list r (reverse log)))

(define-syntax-rule (case name e expected)
  (test-case (symbol->string 'name)
    (check-equal? (with-log (λ () e)) 'expected)))

(define p (make-parameter 'outer))

;; ---- values ----
(case zero-values (events (λ () (amb (values) 1)))
  ((() (1)) ()))
(case multiple-values (events (λ () (amb (values 1 2) 3)))
  (((1 2) (3)) ()))
(case make-amb-multiple (events (λ () (make-amb (λ () (values (list 1 2) (λ () (values '() end)))))))
  (((1 2) ()) ()))

;; ---- dynamic-wind ----
(case wind-choice
  (events (λ () (dynamic-wind (λ () (log! 'in)) (λ () (amb 1 2)) (λ () (log! 'out)))))
  (((1) (2)) (in out in out in out)))
(case wind-pause
  (events (λ () (dynamic-wind (λ () (log! 'in)) (λ () (amb-pause) (log! 'resumed) 'x) (λ () (log! 'out)))))
  ((P (x)) (in out in resumed out)))
(case wind-interleave
  (parameterize ([current-amb-merger amb-interleave])
    (events (λ () (dynamic-wind (λ () (log! 'in))
                                (λ () (amb (begin (amb-pause) 'a) 'b))
                                (λ () (log! 'out))))))
  ((P (b) (a)) (in out in out in out in out)))
(case wind-fail
  (events (λ () (dynamic-wind (λ () (log! 'in)) (λ () (amb-fail)) (λ () (log! 'out)))))
  (() (in out)))
(case wind-raise-after-resume
  (events (λ () (dynamic-wind (λ () (log! 'in)) (λ () (amb-pause) (error 'alt "boom")) (λ () (log! 'out)))))
  ((error "alt: boom") (in out in out)))
(case wind-post
  (events (λ () (dynamic-wind void (λ () (amb-pause) 'x) (λ () (log! 'post)))))
  ((P (x)) (post post)))

;; ---- parameters, marks, breaks ----
(case parameter-after-resume
  (events (λ () (parameterize ([p 'inner]) (amb-pause) (p))))
  ((P (inner)) ()))
(case parameter-in-alternatives
  (events (λ () (parameterize ([p 'inner]) (amb (p) (begin (amb-pause) (p))))))
  (((inner) P (inner)) ()))
(case parameter-in-supply
  (events (λ () (parameterize ([p 'inner])
                  (make-amb (λ () (log! (p)) (values (list 1) end))))))
  (((1)) (outer)))
(case mark-after-resume
  (events (λ () (with-continuation-mark 'key 'inner
                  (begin0 (begin (amb-pause) (continuation-mark-set-first #f 'key)) (void)))))
  ((P (inner)) ()))
(case mark-in-supply
  (events (λ () (with-continuation-mark 'key 'inner
                  (begin0 (make-amb (λ () (log! (continuation-mark-set-first #f 'key))
                                       (values (list 1) end)))
                          (void)))))
  (((1)) (#f)))
(case break-after-resume
  (events (λ () (parameterize-break #f (amb-pause) (break-enabled))))
  ((P (#f)) ()))

;; ---- the boundary of the search ----
(case prompt-in-supply-top
  (events (λ () (make-amb (λ () (log! (continuation-prompt-available? amb-prompt-tag))
                             (values (list 1) end)))))
  (((1)) (#f)))
(case prompt-in-supply-nested
  (events (λ () (amb-pause)
             (events (λ () (make-amb (λ () (log! (continuation-prompt-available? amb-prompt-tag))
                                        (values (list 1) end)))))))
  ((P (((1)))) (#t)))
(case prompt-in-body (events (λ () (amb 1 2) (continuation-prompt-available? amb-prompt-tag)))
  (((#t) (#t)) ()))
(case prompt-after-resume (events (λ () (amb-pause) (continuation-prompt-available? amb-prompt-tag)))
  ((P (#t)) ()))
(case merger-effect
  (let ([pausing (λ (s t) (λ () (amb-pause) ((amb-append s t))))])
    (events (λ () (parameterize ([current-amb-merger pausing])
                    (events (λ () (amb 1 2)))))))
  ((P P (((1) (2)))) ()))
(case merger-effect-top
  (let ([pausing (λ (s t) (λ () (amb-pause) ((amb-append s t))))])
    (parameterize ([current-amb-merger pausing])
      (events (λ () (amb 1 2)))))
  ((error "amb-pause: not in a search") ()))
(case effect-in-supply
  (events (λ () (make-amb (λ () (amb-pause) (values (list 1) end)))))
  ((error "amb-pause: not allowed while the alternatives of a choice are supplied;") ()))
(case effect-in-shuffler
  (events (λ () (parameterize ([current-amb-shuffler (λ (v) (amb-fail))]) (amb 1 2))))
  ((error "amb-fail: not allowed while the alternatives of a choice are supplied;") ()))

;; ---- exceptions ----
(case raise-in-alternative
  (events (λ () (with-handlers ([exn:fail? (λ (e) 'caught-inside)])
                  (amb (error "a") 2))))
  (((caught-inside) (2)) ()))
(case raise-after-resume-caught-outside
  (with-handlers ([exn:fail? (λ (e) 'caught-outside)])
    (events (λ () (amb-pause) (error "b"))))
  (caught-outside ()))
(case handler-inside-after-resume
  (events (λ () (with-handlers ([exn:fail? (λ (e) (list 'caught (exn-message e)))])
                  (amb-pause)
                  (error "c"))))
  ((P ((caught "c"))) ()))

;; ---- barriers ----
(define-syntax-rule (in-barrier e) (call-with-continuation-barrier (λ () e)))
;; An exception that the handler of `raise` itself raises goes to the
;; error display and escape handlers, not to `with-handlers`; log it.
(define-syntax-rule (in-exception-handler e)
  (let/ec esc
    (parameterize ([error-display-handler
                    (λ (msg x) (log! (list 'reported (first-line msg))))]
                   [error-escape-handler (λ () (esc 'escaped))])
      (call-with-exception-handler (λ (_) e) (λ () (raise 'x))))))
(case barrier-fail (events (λ () (in-barrier (amb-fail))))
  (() ()))
(case barrier-empty-amb (events (λ () (in-barrier (amb))))
  ((error "call-with-composable-continuation: cannot capture past continuation barrier") ()))
(case barrier-empty-for/amb (events (λ () (in-barrier (for/amb ([i '()]) i))))
  ((error "call-with-composable-continuation: cannot capture past continuation barrier") ()))
(case barrier-empty-for/amb-length (events (λ () (in-barrier (for/amb #:length 0 ([i '()]) i))))
  ((error "call-with-composable-continuation: cannot capture past continuation barrier") ()))
(case empty-amb-outside (amb)
  ((error "make-amb: not in a search") ()))
(case barrier-empty-sequence (events (λ () (in-barrier (sequence->amb '()))))
  ((error "call-with-composable-continuation: cannot capture past continuation barrier") ()))
(case barrier-choice (events (λ () (in-barrier (amb 1 2))))
  ((error "call-with-composable-continuation: cannot capture past continuation barrier") ()))
(case barrier-pause (events (λ () (in-barrier (amb-pause))))
  ((error "call-with-composable-continuation: cannot capture past continuation barrier") ()))
(case handler-fail (events (λ () (in-exception-handler (amb-fail))))
  (() ()))
(case handler-choice (events (λ () (in-exception-handler (amb 1 2))))
  (((escaped)) ((reported "exception raised by exception handler: call-with-composable-continuation: cannot capture past continuation barrier; original raise called (with non-exception value): 'x"))))

;; ---- sequences of effects ----
(case resume-then-choose (events (λ () (amb-pause) (amb 1 2)))
  ((P (1) (2)) ()))
(case choose-then-pause-then-fail (events (λ () (define x (amb 1 2 3)) (amb-pause) (if (= x 2) (amb-fail) x)))
  ((P (1) P P (3)) ()))
(case nested-search (events (λ () (define x (amb 1 2)) (list x (events (λ () (amb-pause) (amb 'a 'b))))))
  ((((1 (P (a) (b)))) ((2 (P (a) (b))))) ()))
