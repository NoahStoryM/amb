#lang typed/racket/base/shallow

;; ===================================================================
;; Public API for `typed-amb` library
;; ===================================================================
;;
;; This module re-exports the typed core from `typed/amb/base` and
;; defines the choice and observation forms in a typed context.
;;
;; In a `for` clause, `in-amb` and `in-amb*` iterate a single-pass
;; sequence directly. In expression position they produce a lazy stream.

(require typed/racket/unsafe
         (for-syntax racket/base syntax/parse)
         racket/vector
         "base.rkt"
         (only-in "private/types.rkt" ⊥))

(provide Generatorof Merger
         make-amb amb-generate* amb-fail amb-pause
         current-amb-merger
         with-amb-mark call-with-amb-mark
         amb for/amb for*/amb
         amb-local amb-local*
         sequence->amb
         current-amb-shuffler
         (rename-out [*in-amb in-amb] [*in-amb* in-amb*])
         in-amb/do in-amb*/do)


;; ---- Surface procedures ----
;; The untyped implementation is a trusted boundary. These declarations
;; supply static types without installing another layer of run-time contracts.

(unsafe-require/typed amb/main
  [current-amb-shuffler (Parameter (→ Mutable-VectorTop Void))]
  [sequence->amb
   (∀ (a ...)
      (→ (Sequenceof a ... a)
         (Values a ... a)))]
  [amb-local*
   (∀ (a ...)
      (→ (→ (Values a ... a))
         (Values a ... a)))]
  ;; The observer sequence type includes the results of the body and
  ;; the optional pause handler.
  [in-amb*
   ;; The type variables c and d come first, so an explicit
   ;; instantiation must supply them: (inst in-amb* c d a ...).
   (∀ (c d a ...)
      (case→ (→* ((→ ⊥)) (False)
                 (∀ (b ...) (Sequenceof b ... b)))
             (→* ((→ (Values a ... a))) (False)
                 (Sequenceof a ... a))
             (→ (→ c) (Option (→ d)) (Sequenceof (∪ c d)))
             (→ (→ (Values a ... a)) (Option (→ (Values a ... a)))
                (Sequenceof a ... a))))]
  [in-amb*/do
   (∀ (c d a ...)
      (case→ (→* ((→ ⊥)) (False)
                 (∀ (b ...) (Sequenceof b ... b)))
             (→* ((→ (Values a ... a))) (False)
                 (Sequenceof a ... a))
             (→ (→ c) (Option (→ d)) (Sequenceof (∪ c d)))
             (→ (→ (Values a ... a)) (Option (→ (Values a ... a)))
                (Sequenceof a ... a))))])


;; ---- Shared runtime support for the syntax ----

(unsafe-require/typed amb/private/choice
  [thunks
   (∀ (a ...)
      (→ (→ (Values a ... a))
         (→ (Values a ... a))
         *
         (Mutable-Vectorof (→ (Values a ... a)))))]
  [choose
   (∀ (a ...)
      (→ (Mutable-Vectorof (→ (Values a ... a)))
         (→ (Values a ... a))))]
  [choose-none (→ ⊥)]
  [make-choice-generator
   (∀ (a) (→ (→ (→ a Void) Void) (Generatorof (List a))))])

;; ---- amb Syntax ----

(define-syntax amb
  (syntax-rules ()
    [(_)
     (choose-none)]
    [(_ expr* ...)
     ((choose (thunks (λ () expr*) ...)))]))


;; ---- amb-local Syntax ----

;; Identical to the untyped version; defined here so that it expands in
;; a typed context.
(define-syntax-rule (amb-local expr)
  (amb-local* (λ () expr)))


;; ---- for/amb: Nondeterministic Choice over Sequences ----
;;
;; Three syntax patterns are supported:
;;
;;   (for/amb : T₁ #:length n (clauses ...) : T₂ body ...)
;;     The alternatives are collected first, as by `for/vector`: at
;;     most n of them.  `T₁` is the outer result type; `T₂` is the body
;;     type (must be a subtype of `T₁`).
;;
;;   (for/amb : T₁ (clauses ...) : T₂ body ...)
;;     The iterations go on as the search does.
;;
;;   (for/amb (clauses ...) body ...)
;;     No type annotations — defaults to `AnyValues` for both `T₁` and
;;     `T₂`, and re-dispatches to one of the above.
;;
;; Result annotations, including `(Values T₁ T₂ ...)`, are put directly
;; on the body thunks and their vector or producer.
;;
;; Two compilation strategies:
;;
;; 1. With `#:length` — Collect the body thunks into a typed vector via
;;    `for/vector`, counting the iterations; the slots that no
;;    iteration fills hold `amb-fail` and are cut off.  With `#:fill`,
;;    they are alternatives; the fill thunk is annotated as `(→ T₁)`.
;;
;; 2. Without `#:length` — As in the untyped `for/amb`, the loop is a
;;    coroutine, which is the generator of `make-amb`.  The
;;    loop runs under a prompt of its own; at each iteration it captures
;;    its continuation as `retry` and aborts to the prompt with the body
;;    thunk.  The next call of the generator re-enters the loop through
;;    `retry`.

(define-syntaxes (for/amb for*/amb)
  (let ()
    (define-splicing-syntax-class length-clause
      [pattern (~seq #:length n:expr (~optional (~seq #:fill fill:expr)))])
    (define-splicing-syntax-class break-clause
      [pattern (~seq (~or* #:break #:final) guard:expr)])
    (define ((make-for/amb for/vector for) stx)
      (syntax-parse stx
        #:datum-literals (:)
        [(_ : t1:expr (clause ...) : t2:expr break:break-clause ... body ...+)
         #:with (break* ...) #'((~@ . break) ...)
         (quasisyntax/loc stx
           (((inst make-amb (→ t1))
             ((inst make-choice-generator (→ t1))
              (λ (yield)
                (#,for (clause ... break* ...)
                  (yield (ann (λ () : t2 body ...) (→ t1)))))))))]
        [(_ : t1:expr #:length n (clause ...) : t2:expr
            break:break-clause ... body ...+)
         #:with (break* ...) #'((~@ . break) ...)
         (quasisyntax/loc stx
           (let ([m (with-amb-mark n)])
             (if (eqv? m 0)
                 (choose-none)
                 ((choose
                   (with-amb-mark
                    (let* ([count : Integer 0]
                           [thk* (#,for/vector
                                  : (Mutable-Vectorof (→ t1))
                                  #:length m #:fill amb-fail
                                  (clause ... break* ...)
                                  (set! count (add1 count))
                                  (ann (λ () : t2 body ...) (→ t1)))])
                      (if (= count (vector-length thk*))
                          thk*
                          (vector-copy thk* 0 count)))))))))]
        [(_ : t1:expr #:length n #:fill fill-expr
            (clause ...) : t2:expr break:break-clause ... body ...+)
         #:with (break* ...) #'((~@ . break) ...)
         (quasisyntax/loc stx
           (let ([m (with-amb-mark n)])
             (if (eqv? m 0)
                 (choose-none)
                 ((choose
                   (with-amb-mark
                    (#,for/vector
                     : (Mutable-Vectorof (→ t1))
                     #:length m #:fill (ann (λ () : t2 fill-expr) (→ t1))
                     (clause ... break* ...)
                     (ann (λ () : t2 body ...) (→ t1)))))))))]
        [(~or* (name : t0:expr (~optional length:length-clause) (clause ...) break:break-clause ... body ...+)
               (name (~optional length:length-clause) (clause ...) : t0:expr break:break-clause ... body ...+)
               (name (~optional length:length-clause) (clause ...) break:break-clause ... body ...+))
         #:with t (if (attribute t0) #'t0 #'AnyValues)
         #:with (maybe-length ...) (if (attribute length) #'length #'())
         #:with (break* ...) #'((~@ . break) ...)
         ((make-for/amb for/vector for)
          (quasisyntax/loc stx
            (name : t maybe-length ... (clause ...) : t break* ... body ...)))]))
    (values (make-for/amb #'for/vector  #'for)
            (make-for/amb #'for*/vector #'for*))))


;; ---- in-amb* (function form) ----

;; `*in-amb*` is the public binding for `in-amb*`.
;;
;; As an expression: falls through to `in-amb*`, returning a
;; lazy stream.
;;
;; Inside a `for` clause: dispatches to `in-amb*/do` for direct
;; `make-do-sequence` iteration, avoiding stream overhead.
(define-sequence-syntax *in-amb*
  (λ () #'in-amb*)
  (λ (stx)
    (syntax-parse stx
      [[(id:id ...) (_ expr)]
       (syntax/loc stx
         [(id ...)
          (in-amb*/do expr)])]
      [[(id:id ...) (_ expr handler)]
       (syntax/loc stx
         [(id ...)
          (in-amb*/do expr handler)])])))


;; ---- in-amb (macro form) ----

;; Transformer for the expression position: wraps the body in a thunk
;; and calls `in-amb*`.
(define-for-syntax (in-amb stx)
  (syntax-parse stx
    [(_ expr)
     (syntax/loc stx
       (in-amb* (λ () expr)))]
    [(_ expr #:on-pause handler)
     (syntax/loc stx
       (in-amb* (λ () expr) handler))]))

;; `*in-amb` is the public binding for `in-amb`.
;;
;; As an expression: expands via `in-amb` above, returning a
;; lazy stream.
;;
;; Inside a `for` clause: wraps the body in a thunk
;; and dispatches directly to `in-amb*/do`.
(define-sequence-syntax *in-amb
  in-amb
  (λ (stx)
    (syntax-parse stx
      [[(id:id ...) (_ expr)]
       (syntax/loc stx
         [(id ...)
          (in-amb*/do (λ () expr))])]
      [[(id:id ...) (_ expr #:on-pause handler)]
       (syntax/loc stx
         [(id ...)
          (in-amb*/do (λ () expr) handler)])])))


;; ---- in-amb/do (macro form, sequence only) ----

;; Always produces a `make-do-sequence` sequence (never a stream).
;; Intended for cases where only `for`-style iteration is needed.
(define-syntax (in-amb/do stx)
  (syntax-parse stx
    [(_ expr)
     (syntax/loc stx
       (in-amb*/do (λ () expr)))]
    [(_ expr #:on-pause handler)
     (syntax/loc stx
       (in-amb*/do (λ () expr) handler))]))
