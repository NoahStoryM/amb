#lang racket/base

;; ===================================================================
;; Public API for the `amb` library
;; ===================================================================
;;
;; amb/base has the effects of a search; this module adds the syntax
;; that makes the alternatives of a choice, and the ways to observe a
;; search.
;;
;; `in-amb` and `in-amb*` each have two roles:
;;
;;   - as an expression (outside `for`): a stream, which remembers the
;;     answers it has produced;
;;
;;   - in a `for` clause: the single-pass sequence of `in-amb*/do`
;;     directly, without the stream.

(require (for-syntax racket/base syntax/parse)
         racket/contract/base
         racket/stream
         racket/vector
         "base.rkt"
         (rename-in "private/observe.rkt"
                    [in-amb*    -in-amb*]
                    [in-amb*/do -in-amb*/do]))

;; Module-boundary contracts (rather than `contract-out`): the module
;; that uses `in-amb*` / `in-amb*/do` -- including through the `for`
;; clause expansion below -- is the one blamed for a bad argument.
(define-module-boundary-contract in-amb*
  -in-amb*    (->* ((-> any)) ((or/c #f (-> any))) stream?))
(define-module-boundary-contract in-amb*/do
  -in-amb*/do (->* ((-> any)) ((or/c #f (-> any))) sequence?))

(provide (all-from-out "base.rkt")
         amb for/amb for*/amb
         sequence->amb
         amb-local amb-local*
         current-amb-shuffler
         (rename-out [*in-amb in-amb] [*in-amb* in-amb*])
         in-amb/do in-amb*/do)


;; ---- Choices ----

;; An element of the sequence: a choice among all of them.
(define (sequence->amb seq)
  (unless (sequence? seq)
    (raise-argument-error 'sequence->amb "sequence?" seq))
  (make-amb (λ () (sequence-generate* seq))))

;; (amb-local expr) is an answer of expr, searched for on its own: the
;; choices of expr are merged with each other before the rest of the
;; computation sees any of its answers.  The answers of the search of
;; expr are the alternatives of one choice.
(define (amb-local* thk)
  (unless (and (procedure? thk) (procedure-arity-includes? thk 0))
    (raise-argument-error 'amb-local* "(-> any)" thk))
  (make-amb (λ () (amb-generate* thk))))

(define-syntax-rule (amb-local expr)
  (amb-local* (λ () expr)))

;; Called once on the vector of the alternatives of every `amb` and
;; every `for/amb` with `#:length`, where the choice is made, to put
;; them in another order.  The vector may be empty.
(define current-amb-shuffler
  (make-parameter
   void
   (λ (shuffle!)
     (unless (and (procedure? shuffle!) (procedure-arity-includes? shuffle! 1))
       (raise-argument-error 'current-amb-shuffler
                             "(-> mutable-vector? void?)"
                             shuffle!))
     shuffle!)
   'current-amb-shuffler))

;; A choice among the thunks of the mutable vector `thk*`; the chosen
;; thunk is returned, to be called where the choice was made.
(define (choose thk*)
  (with-amb-mark ((current-amb-shuffler) thk*))
  (sequence->amb thk*))

;; (amb e ...) is a choice among the expressions: none of them is
;; evaluated before it is chosen.  (amb) fails.
(define-syntax-rule (amb expr ...)
  ((choose (vector (λ () expr) ...))))

;; Turn a producer into a single-pass generator.  The producer calls
;; yield once for each alternative; each next call resumes it there.
;; Keeping this coroutine here leaves the loop's bindings to the macro.
(define (make-choice-generator produce)
  (define tag (make-continuation-prompt-tag 'for/amb))
  (define retry #f)
  (define (yield alt)
    (define choice (call/cc values tag))
    (when choice
      (set! retry choice)
      (abort-current-continuation tag (list alt))))
  (define (next)
    (values
     (call-with-continuation-prompt
      (λ ()
        (when retry (retry #f))
        (produce yield)
        #f)
      tag values)
     next))
  next)

;; (for/amb (clause ...) body ...+) is a choice among the values of the
;; body, one alternative for each iteration; the body runs where the
;; choice was made.
;;
;;   with #:length n   the alternatives are collected first, as by
;;                     `for/vector`: at most n of them.  With `#:fill`,
;;                     the slots that are left are alternatives too,
;;                     each with the value of the fill expression.
;;   without           the iterations go on as the search does.  The
;;                     loop is a coroutine, which is the generator of
;;                     `make-amb`: it runs under a prompt of its own;
;;                     at each iteration it captures its continuation
;;                     as `retry` and aborts to the prompt with the
;;                     thunk of the body.  The next call of the
;;                     generator goes back into the loop by `retry`.
(define-syntaxes (for/amb for*/amb)
  (let ()
    (define-splicing-syntax-class break-clause
      [pattern (~seq (~or* #:break #:final) guard:expr)])
    (define ((make-for/amb for/vector for) stx)
      (syntax-parse stx
        [(_ (clause ...) break:break-clause ... body ...+)
         #:with (break* ...) #'((~@ . break) ...)
         (quasisyntax/loc stx
           ((make-amb
             (make-choice-generator
              (λ (yield)
                (#,for (clause ...) break* ...
                  (yield (λ () body ...))))))))]
        ;; The slots that no iteration fills hold `amb-fail`, and are
        ;; cut off: the iterations are counted, so no value in the
        ;; vector has to be told from an alternative.
        [(_ #:length n (clause ...) break:break-clause ... body ...+)
         #:with (break* ...) #'((~@ . break) ...)
         (quasisyntax/loc stx
           ((choose
             (with-amb-mark
              (let* ([count 0]
                     [thk* (#,for/vector #:length n #:fill amb-fail
                            (clause ...) break* ...
                            (set! count (add1 count))
                            (λ () body ...))])
                (if (= count (vector-length thk*))
                    thk*
                    (vector-copy thk* 0 count)))))))]
        [(_ #:length n #:fill fill-expr
            (clause ...) break:break-clause ... body ...+)
         #:with (break* ...) #'((~@ . break) ...)
         (quasisyntax/loc stx
           ((choose
             (with-amb-mark
              (#,for/vector #:length n #:fill (λ () fill-expr)
               (clause ...) break* ... (λ () body ...))))))]))
    (values (make-for/amb #'for/vector  #'for)
            (make-for/amb #'for*/vector #'for*))))

;; ---- in-amb* (function form) ----

;; Validate the arguments before calling the private `in-amb*/do`: `thk`
;; must be a thunk, and the pause handler a thunk or #f.  If they are
;; not, call the contracted `in-amb*` only to raise its contract error;
;; it never runs the search.
;;
;; `check-args` is a macro, not a function, so that the contracted call
;; is part of the expansion at the use site: the module using `in-amb*`
;; is blamed, not this module.
(define-syntax-rule (check-args thk handler)
  (unless (and (procedure? thk)
               (procedure-arity-includes? thk 0)
               (or (not handler)
                   (and (procedure? handler)
                        (procedure-arity-includes? handler 0))))
    (in-amb* thk handler)))

;; `*in-amb*` is the public binding for `in-amb*`.  The function forms
;; take the pause handler as an optional argument, not by keyword, as
;; the empty handler of v2 was: a keyword argument costs at every call.
(define-sequence-syntax *in-amb*
  (λ () #'in-amb*)
  (λ (stx)
    (syntax-parse stx
      [[(id:id ...) (_ expr (~optional handler-expr))]
       #:with handler-e (or (attribute handler-expr) #'#f)
       (syntax/loc stx
         [(id ...)
          (let ([thk expr] [handler handler-e])
            (check-args thk handler)
            (-in-amb*/do thk handler))])])))


;; ---- in-amb (macro form) ----

;; `*in-amb` is the public binding for `in-amb`.
(define-sequence-syntax *in-amb
  (λ (stx)
    (syntax-parse stx
      [(_ expr)
       (syntax/loc stx (-in-amb* (λ () expr)))]
      [(_ expr #:on-pause handler)
       (syntax/loc stx (in-amb* (λ () expr) handler))]))
  (λ (stx)
    (syntax-parse stx
      [[(id:id ...) (_ expr)]
       (syntax/loc stx
         [(id ...) (-in-amb*/do (λ () expr))])]
      [[(id:id ...) (_ expr #:on-pause handler)]
       (syntax/loc stx
         [(id ...) (in-amb*/do (λ () expr) handler)])])))


;; ---- in-amb/do (macro form, sequence only) ----

;; Always the single-pass sequence, never a stream.
(define-syntax (in-amb/do stx)
  (syntax-parse stx
    [(_ expr)
     (syntax/loc stx (in-amb*/do (λ () expr)))]
    [(_ expr #:on-pause handler)
     (syntax/loc stx (in-amb*/do (λ () expr) handler))]))
