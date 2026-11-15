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
;; A generator is called once.  The procedure returned here can be
;; called again only when it did not return: the sequence and the
;; stream ask again for an element that they did not get.
;;
;;   - A pause is over before its handler is called: if the handler
;;     raises, the next call goes on after the pause.
;;
;;   - If the search itself does not return (it raises, or control
;;     leaves it), what it has run cannot be run again: the next call
;;     raises.
(define (elements gen handler)
  (λ ()
    (let loop ([gen* (begin0 gen (set! gen #f))])
      (unless gen*
        (raise
         (exn:fail:contract
          (string-append
           "in-amb: the search cannot go on;\n"
           " its last step did not return: it raised an exception, or control left it")
          (current-continuation-marks))))
      (let-values ([(v next) (gen*)])
        (cond
          [(list? v) (values v (elements next handler))]
          [(not v) (values #f raise-empty-sequence-error)]
          [(pause-count? v)
           (if handler
               (let ([next (pauses (sub1 v) next)])
                 (set! gen next)
                 (values (call-with-values handler list)
                         (elements next handler)))
               (loop next))]
          [else (raise-result-error 'in-amb v)])))))

(define ((make make-sequence) thk [handler #f])
  (make-sequence (elements (λ () (amb-generate* thk)) handler)))

(define in-amb*    (make make-stream))
(define in-amb*/do (make make-sequence))
