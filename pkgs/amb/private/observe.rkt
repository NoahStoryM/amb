#lang racket/base

;; ===================================================================
;; Observing a search
;; ===================================================================
;;
;; The answers of a search, as a stream that remembers them (`in-amb*`)
;; or as a single-pass sequence (`in-amb*/do`).  Neither runs anything
;; of the search before its first element is asked for.
;;
;; The pauses of the search are skipped, unless there is a handler: it
;; is called once for each pause, and the values it returns are an
;; element, in the place of the pause.  They do not go back into the
;; search.

(require "core.rkt"
         "generator.rkt"
         "data/sequence.rkt"
         "data/stream.rkt")

(provide in-amb* in-amb*/do)

(define (raise-interrupted-search)
  (raise
   (exn:fail:contract
    (string-append
     "in-amb: the search cannot go on;\n"
     " its last step has not returned: it is still running,\n"
     " it raised an exception, or control escaped from it")
    (current-continuation-marks))))

;; The elements of the generator `gen`, in the shape that
;; `sequence-generate*` returns: (values (or/c list? #f) next).
;;
;; A generator is called once.  The procedure returned here returns
;; once; the sequence and the stream (data/stream.rkt keeps no delay
;; that an exception would break) call it again only to ask again for
;; an element that they did not get.  What it does then depends on why
;; they did not get it:
;;
;;   - The search did not return: it raised, or control left it; or it
;;     is still running, and asks for its own next element.  What it
;;     has run cannot be run again: the call raises.
;;
;;   - The handler of a pause did not return: it raised, or control
;;     left it.  The pause is over: the call goes on after it.
;;
;;   - The handler of a pause is still running, and asks for the
;;     element that it is producing.  The call raises, and the handler
;;     goes on.
;;
;; The state:
;;
;;   gen       the generator to call; #f from the moment it is called
;;   resume    #f; or, from the moment the handler of a pause is called
;;             until it returns, the generator of what comes after the
;;             pause
;;   running?  whether control is in the handler
;;
;; The last two cases both have `resume`, and `running?` tells them
;; apart.  It is kept by a `dynamic-wind` around the call of the
;; handler, so it follows control itself: it is cleared when an
;; exception or a jump leaves the handler, whatever prompts are in
;; between.  (A continuation mark would not do: a prompt between the
;; handler and the read hides it, and "no mark" would be taken for
;; "the handler was left".)  Only a pause that has a handler pays for
;; it; a read that finds no `resume` tests one variable.
(define (elements gen handler)
  (define resume #f)
  (define running? #f)
  (define (next)
    (when resume
      (when running?
        (raise
         (exn:fail:contract
          (string-append
           "in-amb: an element cannot be read by the handler of its own pause;\n"
           " the handler of that pause is still running")
          (current-continuation-marks))))
      (set! gen resume)
      (set! resume #f))
    (let loop ([gen* (begin0 gen (set! gen #f))])
      (unless gen* (raise-interrupted-search))
      (let-values ([(v after) (gen*)])
        (cond
          [(list? v) (values v (elements after handler))]
          [(not v) (values #f raise-empty-sequence-error)]
          [(exact-positive-integer? v)
           (if handler
               (let ([after (pauses (sub1 v) after)])
                 (set! resume after)
                 (let ([vs (dynamic-wind
                            (λ () (set! running? #t))
                            (λ () (call-with-values handler list))
                            (λ () (set! running? #f)))])
                   (set! resume #f)
                   (values vs (elements after handler))))
               (loop after))]
          [else (raise-result-error 'in-amb v)]))))
  next)

(define (in-amb* thk [handler #f])
  (make-stream (elements (λ () (amb-generate* thk)) handler)))

;; The sequence holds the raw generator, so this function only needs to
;; find the next answer and can return its successor directly.
(define (next-answer gen)
  (unless gen (raise-interrupted-search))
  (let loop ([gen gen])
    (let-values ([(v after) (gen)])
      (cond
        [(list? v) (values v after)]
        [(not v) (values #f raise-empty-sequence-error)]
        [(exact-positive-integer? v) (loop after)]
        [else (raise-result-error 'in-amb v)]))))

(define (in-amb*/do thk [handler #f])
  (define (gen) (amb-generate* thk))
  (if handler
      (make-sequence (elements gen handler))
      (make-sequence gen next-answer)))
