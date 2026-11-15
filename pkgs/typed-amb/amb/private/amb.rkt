#lang typed/racket/base/shallow

;; ===================================================================
;; Typed Front-End for `amb`
;; ===================================================================
;;
;; This module gives types to the public bindings of `amb/base`,
;; `amb/merge` and `amb/main`, and defines typed versions of the
;; syntactic forms (`amb`, `for/amb`, `in-amb`, ...), which cannot be
;; imported with `require/typed`.  Runtime helpers shared with amb/main
;; are imported from amb/private/choice; their implementations stay in
;; the untyped package.
;;
;; Uses `typed/racket/base/shallow` for lower contract overhead at the
;; typed-untyped boundary.

(require (for-syntax racket/base syntax/parse)
         racket/vector)

(provide Generatorof Generator Merger
         amb amb-local
         for/amb for*/amb)


;; ---- The generator protocol ----
;;
;;   Generator = () -> (values Result Generator)
;;   Result    = list? | #f | exact-positive-integer?
;;
;; (Generatorof a) is a generator whose answers have the type a, a type
;; of lists: an answer is the list of the values of one element.
;;
;; A merger does not know the answers of the search that it merges in:
;; the choice that calls it is made before the search has any.  So the
;; type of the mergers of `current-amb-merger` is that of generators of
;; any answers.  The standard mergers are polymorphic, and so they are
;; mergers in this sense too, and keep the type of the generators that
;; typed code applies them to.
(define-type (Generatorof a)
  (→ (Values (∪ a #f Positive-Integer) (Generatorof a))))
(define-type Generator (Generatorof (Listof Any)))
(define-type Merger (→ Generator Generator Generator))

(define-type ⊥ Nothing)


;; ---- Typed Re-Exports ----

(require/typed/provide amb/merge
  [amb-append     (∀ (a) (→ (Generatorof a) (Generatorof a) (Generatorof a)))]
  [amb-interleave (∀ (a) (→ (Generatorof a) (Generatorof a) (Generatorof a)))]
  [amb-alternate  (∀ (a) (→ (Generatorof a) (Generatorof a) (Generatorof a)))]
  [amb-level      (∀ (a) (→ (Generatorof a) (Generatorof a) (Generatorof a)))])

;; `in-amb*` and `in-amb*/do` take an optional pause handler; the
;; sequence type is the union of the body's type and the handler's.
(require/typed/provide amb
  [make-amb
   (∀ (a ...)
      (→ (Generatorof (List a ... a))
         (Values a ... a)))]
  [amb-generate*
   (∀ (a ...)
      (→ (→ (Values a ... a))
         (Values (∪ (List a ... a) #f Positive-Integer)
                 (Generatorof (List a ... a)))))]
  [amb-fail  (→ ⊥)]
  [amb-pause (→ Void)]
  [call-with-amb-mark
   (∀ (a ...)
      (case→ (→ (→ ⊥) ⊥)
             (→ (→ (Values a ... a)) (Values a ... a))))]
  [current-amb-merger   (Parameter Merger)]
  [current-amb-shuffler (Parameter (→ Mutable-VectorTop Void))]
  [sequence->amb
   (∀ (a ...)
      (→ (Sequenceof a ... a)
         (Values a ... a)))]
  [amb-local*
   (∀ (a ...)
      (→ (→ (Values a ... a))
         (Values a ... a)))]
  [in-amb*
   ;; The type variables c and d come first, so an explicit
   ;; instantiation must supply them: (inst in-amb* c d a ...).
   (∀ (c d a ...)
      (case→ (→ (→ ⊥)
                (∀ (b ...) (Sequenceof b ... b)))
             (→ (→ (Values a ... a))
                (Sequenceof a ... a))
             (→ (→ c) (→ d) (Sequenceof (∪ c d)))
             (→ (→ (Values a ... a)) (→ (Values a ... a))
                (Sequenceof a ... a))))]
  [in-amb*/do
   (∀ (c d a ...)
      (case→ (→ (→ ⊥)
                (∀ (b ...) (Sequenceof b ... b)))
             (→ (→ (Values a ... a))
                (Sequenceof a ... a))
             (→ (→ c) (→ d) (Sequenceof (∪ c d)))
             (→ (→ (Values a ... a)) (→ (Values a ... a))
                (Sequenceof a ... a))))])


;; ---- Shared runtime support for the syntax ----

(require/typed/provide amb/private/choice
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

;; Use the typed procedure binding, just as the untyped syntax does.
(provide with-amb-mark)
(define-syntax-rule (with-amb-mark e)
  (call-with-amb-mark (λ () e)))


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

(define-for-syntax (append* v**) (apply append v**))

(define-syntaxes (for/amb for*/amb)
  (let ()
    (define-splicing-syntax-class length-clause
      [pattern (~seq #:length n:expr (~optional (~seq #:fill fill:expr)))])
    (define-splicing-syntax-class break-clause
      [pattern (~seq (~or* #:break #:final) guard:expr)])
    (define (make-for/amb for/vector for)
      (define (parser stx)
        (syntax-parse stx
          #:datum-literals (:)
          [(name
            : t1:expr
            #:length n (~optional (~seq #:fill fill-expr))
            (clauses ...)
            : t2:expr
            break-expr:break-clause ...
            body ...+)
           #:with fill
           (if (attribute fill-expr)
               #'(ann (λ () : t2 fill-expr) (→ t1)) ; ensure `t2 <: t1`
               #'amb-fail)
           #:with keep-all? (if (attribute fill-expr) #'#t #'#f)
           #:do
           [(define break
              (if (attribute break-expr)
                  (append* (map syntax->list
                                (syntax->list #'(break-expr ...))))
                  '()))]
           (quasisyntax/loc stx
             (let ([m (with-amb-mark n)])
               (if (zero? m)
                   (choose-none)
                   ((choose
                     (with-amb-mark
                      (let* ([count : Integer 0]
                            [thk* (#,for/vector
                                   : (Mutable-Vectorof (→ t1))
                                   #:length m #:fill fill
                                   (clauses ...)
                                   : (→ t2)
                                   #,@break
                                   (set! count (add1 count))
                                   (define (alt) : t2 body ...)
                                   (ann alt (→ t1)))])
                       (if (or keep-all? (= count (vector-length thk*)))
                           thk*
                           (vector-copy thk* 0 count)))))))))]
          [(_ : t1:expr (clauses ...) : t2:expr break-expr:break-clause ... body ...+)
           #:do
           [(define break
              (if (attribute break-expr)
                  (append* (map syntax->list
                                (syntax->list #'(break-expr ...))))
                  '()))]
           (quasisyntax/loc stx
             (((inst make-amb (→ t1))
               ((inst make-choice-generator (→ t1))
                (λ (yield)
                  (#,for (clauses ...) #,@break
                   (define (alt) : t2 body ...)
                   (yield (ann alt (→ t1)))))))))]
          [(~or* (name : t0:expr (~optional length:length-clause) (clauses ...) break-expr:break-clause ... body ...+)
                 (name (~optional length:length-clause) (clauses ...) : t0:expr break-expr:break-clause ... body ...+)
                 (name (~optional length:length-clause) (clauses ...) break-expr:break-clause ... body ...+))
           #:with t (if (attribute t0) #'t0 #'AnyValues)
           #:with (maybe-length ...) (if (attribute length) #'length #'())
           #:do
           [(define break
              (if (attribute break-expr)
                  (append* (map syntax->list
                                (syntax->list #'(break-expr ...))))
                  '()))]
           (parser
            (quasisyntax/loc stx
              (name : t maybe-length ... (clauses ...) : t #,@break body ...)))]))
      parser)
    (values (make-for/amb #'for/vector  #'for)
            (make-for/amb #'for*/vector #'for*))))
