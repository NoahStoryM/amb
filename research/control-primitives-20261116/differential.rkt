#lang racket/base

;; Differential cases for the control primitives of private/core.rkt.
;; Prints one line per case; run it under each implementation and
;; compare the outputs:
;;
;;   for v in orig a s0; do AMB_IMPL=$v racket differential.rkt > $v.txt; done
;;
;; (AMB_IMPL only exists on the research branch; on v3 the script runs
;; the implementation that is there.)

(require amb amb/merge
         (only-in amb/private/tag amb-prompt-tag))

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
(define (with-log thk)
  (set! log '())
  (define r (with-handlers ([(λ (_) #t) (λ (e) (list 'error (if (exn? e) (first-line (exn-message e)) e)))]) (thk)))
  (list r (reverse log)))

(define-syntax-rule (case name e)
  (printf "~a: ~s\n" 'name (with-log (λ () e))))

(define p (make-parameter 'outer))

;; ---- values ----
(case zero-values (events (λ () (amb (values) 1))))
(case multiple-values (events (λ () (amb (values 1 2) 3))))
(case make-amb-multiple (events (λ () (make-amb (λ () (values (list 1 2) (λ () (values '() end))))))))

;; ---- dynamic-wind ----
(case wind-choice
  (events (λ () (dynamic-wind (λ () (log! 'in)) (λ () (amb 1 2)) (λ () (log! 'out))))))
(case wind-pause
  (events (λ () (dynamic-wind (λ () (log! 'in)) (λ () (amb-pause) (log! 'resumed) 'x) (λ () (log! 'out))))))
(case wind-interleave
  (parameterize ([current-amb-merger amb-interleave])
    (events (λ () (dynamic-wind (λ () (log! 'in))
                                (λ () (amb (begin (amb-pause) 'a) 'b))
                                (λ () (log! 'out)))))))
(case wind-fail
  (events (λ () (dynamic-wind (λ () (log! 'in)) (λ () (amb-fail)) (λ () (log! 'out))))))
(case wind-raise-after-resume
  (events (λ () (dynamic-wind (λ () (log! 'in)) (λ () (amb-pause) (error 'alt "boom")) (λ () (log! 'out))))))
(case wind-post-escapes
  (events (λ () (let/ec esc
                  (dynamic-wind void
                                (λ () (amb-pause) 'x)
                                (λ () (log! 'post) (when (null? (cdr log)) (void))))))))

;; ---- parameters, marks, breaks ----
(case parameter-after-resume
  (events (λ () (parameterize ([p 'inner]) (amb-pause) (p)))))
(case parameter-in-alternatives
  (events (λ () (parameterize ([p 'inner]) (amb (p) (begin (amb-pause) (p)))))))
(case parameter-in-supply
  (events (λ () (parameterize ([p 'inner])
                  (make-amb (λ () (log! (p)) (values (list 1) end)))))))
(case mark-after-resume
  (events (λ () (with-continuation-mark 'key 'inner
                  (begin0 (begin (amb-pause) (continuation-mark-set-first #f 'key)) (void))))))
(case mark-in-supply
  (events (λ () (with-continuation-mark 'key 'inner
                  (begin0 (make-amb (λ () (log! (continuation-mark-set-first #f 'key))
                                       (values (list 1) end)))
                          (void))))))
(case break-after-resume
  (events (λ () (parameterize-break #f (amb-pause) (break-enabled)))))

;; ---- the boundary of the search ----
(case prompt-in-supply-top
  (events (λ () (make-amb (λ () (log! (continuation-prompt-available? amb-prompt-tag))
                             (values (list 1) end))))))
(case prompt-in-supply-nested
  (events (λ () (amb-pause)
             (events (λ () (make-amb (λ () (log! (continuation-prompt-available? amb-prompt-tag))
                                        (values (list 1) end))))))))
(case prompt-in-body (events (λ () (amb 1 2) (continuation-prompt-available? amb-prompt-tag))))
(case prompt-after-resume (events (λ () (amb-pause) (continuation-prompt-available? amb-prompt-tag))))
(case merger-effect
  (let ([pausing (λ (s t) (λ () (amb-pause) ((amb-append s t))))])
    (events (λ () (parameterize ([current-amb-merger pausing])
                    (events (λ () (amb 1 2))))))))
(case merger-effect-top
  (let ([pausing (λ (s t) (λ () (amb-pause) ((amb-append s t))))])
    (parameterize ([current-amb-merger pausing])
      (events (λ () (amb 1 2))))))
(case effect-in-supply
  (events (λ () (make-amb (λ () (amb-pause) (values (list 1) end))))))
(case effect-in-shuffler
  (events (λ () (parameterize ([current-amb-shuffler (λ (v) (amb-fail))]) (amb 1 2)))))

;; ---- exceptions ----
(case raise-in-alternative
  (events (λ () (with-handlers ([exn:fail? (λ (e) 'caught-inside)])
                  (amb (error "a") 2)))))
(case raise-after-resume-caught-outside
  (with-handlers ([exn:fail? (λ (e) 'caught-outside)])
    (events (λ () (amb-pause) (error "b")))))
(case handler-inside-after-resume
  (events (λ () (with-handlers ([exn:fail? (λ (e) (list 'caught (exn-message e)))])
                  (amb-pause)
                  (error "c")))))

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
(case barrier-fail (events (λ () (in-barrier (amb-fail)))))
(case barrier-empty-amb (events (λ () (in-barrier (amb)))))
(case barrier-empty-sequence (events (λ () (in-barrier (sequence->amb '())))))
(case barrier-choice (events (λ () (in-barrier (amb 1 2)))))
(case barrier-pause (events (λ () (in-barrier (amb-pause)))))
(case handler-fail (events (λ () (in-exception-handler (amb-fail)))))
(case handler-choice (events (λ () (in-exception-handler (amb 1 2)))))

;; ---- sequences of effects ----
(case resume-then-choose (events (λ () (amb-pause) (amb 1 2))))
(case choose-then-pause-then-fail (events (λ () (define x (amb 1 2 3)) (amb-pause) (if (= x 2) (amb-fail) x))))
(case nested-search (events (λ () (define x (amb 1 2)) (list x (events (λ () (amb-pause) (amb 'a 'b)))))))
(case in-amb-handler
  (for/list ([x (in-amb/do (begin (amb-pause) (amb 1 2)) #:on-pause (λ () (log! 'P)))]) x))
