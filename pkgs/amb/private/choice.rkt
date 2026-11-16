#lang racket/base

;; Runtime support for the choice syntax in amb/main and typed/amb.
;; Keep it here so the typed front end only needs to give it types.

(require (only-in racket/vector vector-copy)
         "core.rkt"
         (only-in "generator.rkt" end))

(provide sequence->amb
         amb-local*
         current-amb-shuffler
         thunks choose choose-none
         make-choice-generator
         mutable-vector-generate*
         compact-length)

;; An element of the sequence: a choice among all of them.
(define (sequence->amb seq)
  (unless (sequence? seq)
    (raise-argument-error 'sequence->amb "sequence?" seq))
  (make-amb (λ () (sequence-generate* seq))))

;; Find the answers of a local search before the enclosing computation
;; continues with them.  Its pauses remain pauses of the enclosing search.
(define (amb-local* thk)
  (unless (and (procedure? thk) (procedure-arity-includes? thk 0))
    (raise-argument-error 'amb-local* "(-> any)" thk))
  (make-amb (λ () (amb-generate* thk))))

;; Called once on each vector of alternatives, even an empty vector.
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

;; The typed syntax uses this to infer one result type for its thunks.
(define (thunks thk . thk*)
  (list->vector (cons thk thk*)))

;; Return the chosen thunk; the syntax calls it at the choice point.
;; `thk*` is a vector that the syntax has just made, so no one but the
;; shuffler has seen it, and the slots can be cleared as they are taken.
;; (A vector of a user goes to `sequence->amb`, and is not changed.)
(define (choose thk*)
  (with-amb-mark ((current-amb-shuffler) thk*))
  (make-amb (λ () (mutable-vector-generate* thk*))))

;; (mutable-vector-generate* vec) -> (values Result Generator)
;;
;; As (sequence-generate* vec), for a vector that is the caller's alone:
;; each slot is cleared when its element is taken.  A thunk that has
;; been chosen, and what only it refers to, is then not kept until the
;; last alternative of the choice is supplied, as it would be by the
;; vector that the generators of `sequence-generate*` hold.
;;
;; The generators are single-pass (private/generator.rkt), and since a
;; slot is empty once it is taken, a generator that is called again is
;; an error, not a second supply of the same slots.  The generator that
;; is returned with #f is that of `sequence-generate*`: calling it is
;; the error "sequence has no more values".
;;
;; A long vector is also compacted: once at least half of the slots of
;; a vector of `compact-length` slots or more have been taken, the rest
;; is copied into a new vector, and the old one is dropped.  The copies
;; are at most as long as the vector together, since each is at most
;; half of the one before it.
(define (mutable-vector-generate* vec)
  (define len (vector-length vec))
  (define pos 0)                        ; the slot of the live generator
  (define off 0)                        ; the slot of the original at 0 of vec
  (define ((take i))
    (unless (eq? i pos) (raise-called-again-error))
    (set! pos (add1 i))
    (if (< i len)
        (let* ([j (- i off)]
               [v (vector-ref vec j)]
               [n (vector-length vec)]
               [taken (add1 j)])
          (if (and (>= n compact-length) (>= (* 2 taken) n) (< taken n))
              (begin (set! vec (vector-copy vec taken))
                     (set! off (add1 i)))
              (vector-set! vec j #f))
          (values (list v) (take (add1 i))))
        (end)))
  ((take 0)))

;; The length from which a vector is compacted.
(define compact-length 256)

(define (raise-called-again-error)
  (raise (exn:fail:contract
          (string-append
           "amb: a generator of a choice was called again;\n"
           " a generator, and each generator that it returns, is called at most once;\n"
           " a merger must not call the same generator twice")
          (current-continuation-marks))))

;; There is no slot a shuffler can mutate in this shared vector.
(define mutable-empty-vector (vector))

;; An empty choice is a choice like the others, among no alternatives:
;; the shuffler is called once, and `make-amb` is supplied with nothing.
;; (`amb-fail` is the failure that is not a choice.)
(define (choose-none)
  (choose mutable-empty-vector))

;; Turn a producer into a single-pass generator.  The producer calls
;; yield once for each alternative; each next call resumes it there.
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
