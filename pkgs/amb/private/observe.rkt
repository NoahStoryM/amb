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

(require "../base.rkt"
         "generator.rkt"
         "data/sequence.rkt"
         "data/stream.rkt")

(provide in-amb* in-amb*/do)

;; The elements of the generator `gen`, in the shape that
;; `sequence-generate*` returns: (values (or/c list? #f) next).
(define ((elements gen handler))
  (let loop ([gen gen])
    (let-values ([(v next) (gen)])
      (cond
        [(list? v) (values v (elements next handler))]
        [(not v) (values #f raise-empty-sequence-error)]
        [(pause-count? v)
         (if handler
             (let ([next (pauses (sub1 v) next)])
               (values (call-with-values handler list)
                       (elements next handler)))
             (loop next))]
        [else (raise-result-error 'in-amb v)]))))

(define ((make make-sequence) thk #:on-pause [handler #f])
  (make-sequence (elements (λ () (amb-generate* thk)) handler)))

(define in-amb*    (make make-stream))
(define in-amb*/do (make make-sequence))
