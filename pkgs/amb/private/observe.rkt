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
;;   gen      the generator to call; #f from the moment it is called
;;   resume   #f; or, from the moment the handler of a pause is called
;;            until it returns, the generator of what comes after the
;;            pause
;;
;; The last two cases both have `resume`.  They are told apart by a
;; continuation mark around the call of the handler, which is there
;; exactly as long as the handler runs.  It is looked for only when
;; `resume` is set, so a read that finds neither case pays nothing.
(define handler-key (make-continuation-mark-key 'in-amb-handler))

(define (elements gen handler)
  (define resume #f)
  (define (next)
    (when resume
      (when (memq next
                  (continuation-mark-set->list (current-continuation-marks)
                                               handler-key))
        (raise
         (exn:fail:contract
          (string-append
           "in-amb: an element cannot be read by the handler of its own pause;\n"
           " the handler is still running")
          (current-continuation-marks))))
      (set! gen resume)
      (set! resume #f))
    (let loop ([gen* (begin0 gen (set! gen #f))])
      (unless gen*
        (raise
         (exn:fail:contract
          (string-append
           "in-amb: the search cannot go on;\n"
           " its last step did not return: it is still running, it raised an exception,\n"
           " or control left it")
          (current-continuation-marks))))
      (let-values ([(v after) (gen*)])
        (cond
          [(list? v) (values v (elements after handler))]
          [(not v) (values #f raise-empty-sequence-error)]
          [(pause-count? v)
           (if handler
               (let ([after (pauses (sub1 v) after)])
                 (set! resume after)
                 (let ([vs (with-continuation-mark handler-key next
                             (call-with-values handler list))])
                   (set! resume #f)
                   (values vs (elements after handler))))
               (loop after))]
          [else (raise-result-error 'in-amb v)]))))
  next)

(define ((make make-sequence) thk [handler #f])
  (make-sequence (elements (λ () (amb-generate* thk)) handler)))

(define in-amb*    (make make-stream))
(define in-amb*/do (make make-sequence))
