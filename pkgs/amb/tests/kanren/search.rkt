#lang racket/base

;; ===================================================================
;; What a kanren needs of a search, written with amb/base
;; ===================================================================
;;
;; Only the five bindings of amb/base are used: `make-amb`,
;; `amb-generate*`, `amb-pause`, `amb-fail`, `current-amb-merger`.

(require amb/base)

(provide choice-of local-search first-answer take-answers
         (all-from-out amb/base))

(define (end) (values #f end))

;; One of the values: a choice among them, by the current merger.
(define (choice-of . vs)
  (make-amb
   (let loop ([vs vs])
     (λ ()
       (if (null? vs)
           (end)
           (values (list (car vs)) (loop (cdr vs))))))))

;; An answer of (thk), searched for on its own: the choices of (thk) are
;; merged with each other before the rest of the computation sees any
;; of its answers.  This is the `bind` of a stream.
(define (local-search thk)
  (make-amb (λ () (amb-generate* thk))))

;; The search of (thk), up to its first answer; its pauses on the way
;; are pauses of the search around it.  Returns the answer and the
;; generator of the rest, or #f twice when there is no answer.
(define (first-answer thk)
  (let loop ([gen (λ () (amb-generate* thk))])
    (let-values ([(v next) (gen)])
      (cond
        [(list? v) (values v next)]
        [(not v) (values #f #f)]
        [else (for ([_ (in-range v)]) (amb-pause)) (loop next)]))))

;; The first n answers of (thk), all of them when n is #f; one value
;; each.  Nothing of the search runs after the last answer taken.
(define (take-answers n thk)
  (if (eqv? n 0)
      '()
      (let loop ([gen (λ () (amb-generate* thk))] [n n])
        (let-values ([(v next) (gen)])
          (cond
            [(list? v)
             (cons (car v)
                   (if (eqv? n 1)
                       '()
                       (loop next (and n (sub1 n)))))]
            [(not v) '()]
            [else (loop next n)])))))
