#lang scribble/manual

@(require (for-label racket/base
                     amb
                     amb/base
                     amb/merge)
          "utils.rkt")

@title[#:tag "typed"]{Typed Interface}

@racketmodname[typed/amb], in the package @tt{typed-amb}, gives types
to the bindings of @racketmodname[amb]; @racketidfont{typed/amb/base}
and @racketidfont{typed/amb/merge} do the same for
@racketmodname[amb/base] and @racketmodname[amb/merge].


@section{Types}

@itemlist[
@item{@racket[(Generatorof _a)] is the type of a @tech{generator}
      whose answers have the type @racket[_a]:
      @racket[(→ (Values (∪ _a #f Positive-Integer) (Generatorof _a)))].
      @racket[_a] must be a type of lists, since an answer is the list
      of its values; nothing checks this.}
@item{@racketidfont{Merger} is
      @racket[(∀ (a ...)
                 (→ (Generatorof (List a ... a))
                    (Generatorof (List a ... a))
                    (Generatorof (List a ... a))))],
      and @racket[current-amb-merger] is a @racket[(Parameter Merger)].
      The dotted variables keep the number and the types of all the
      values of an answer.  A typed merger can drop answers, repeat
      them, reorder them and add pauses, but it cannot make up an
      answer of those unknown types.}
@item{@racket[for/amb] and @racket[for*/amb] accept the optional type
      annotations shown in their grammar (@secref["surface"]).}
]

Since an answer is known to be a list, a typed merger can recognize it
with @racket[list?]:

@racketblock[
(: my-interleave Merger)
(define ((my-interleave s t))
  (let-values ([(v s) (s)])
    (cond
      [(list? v) (values v (my-interleave s t))]
      [(not v) (t)]
      [else (values 1 (my-interleave t (pauses (sub1 v) s)))])))

(: pauses (∀ (a) (→ Natural (Generatorof a) (Generatorof a))))
(define (pauses n g)
  (if (zero? n)
      g
      (λ () (values 1 (pauses (sub1 n) g)))))
]

The dotted type describes a fixed list of value types at each
instantiation.  It does not accept a generator whose answers have an
arbitrary, statically unknown length, such as
@racket[(Generatorof (Listof Any))], and the front end does not
provide a separate top type for generators.


@section{The Trusted Boundary}

The typed front end is written in @tt{typed/racket/base/shallow} and
imports the untyped implementations of this library through
@racketidfont{unsafe-require/typed/provide}.  It trusts them to
preserve the declared types, instead of adding run-time contracts to
their own checks of arguments and of the generator protocol.  Shallow
Typed Racket is kept so that the macros expand in every client: macros
defined in Deep Typed Racket are rejected in Shallow clients.  Clients
may use Deep, Shallow or Optional Typed Racket.

This boundary is trusted; it does not protect against arbitrary
untyped code.  The protocol checks tell an answer from a pause or the
end, but they do not establish the types of the values in an answer.
An untyped merger, including one installed through the shared
@racket[current-amb-merger] parameter, must preserve the types of the
answers; if it makes up an answer of another type, the types of a
typed observer no longer hold.  The same holds for an untyped shuffler,
which must leave thunks of the right type in the vector.

Deep @racketidfont{require/typed} cannot generate a contract for the
variable-arity polymorphic @racketidfont{Merger}.  A custom merger can
be written in typed code, or imported through an explicitly trusted
@racketidfont{unsafe-require/typed}.  The standard mergers of the
library use such a trusted boundary and do not seal the answer lists.


@section[#:tag "typed-limitations"]{Known Typed Racket Limitations}

@itemlist[
@item{The type checker of Typed Racket's @racket[for] raises an
      internal error for a clause with no identifiers over a
      zero-valued sequence, including @racket[(in-amb/do (values))].
      Zero-valued computations themselves work: receive their values
      with @racket[define-values] inside the search, and return an
      ordinary element for typed iteration.  Untyped iteration is not
      affected.}
@item{The native typed @racket[for*] and @racket[for*/vector] may
      ignore @racket[#:final], and typed @racket[for*/amb] inherits
      this, with or without @racket[#:length].  For example, a loop
      over @racket['(1 2)] and @racket['(10 20)] with
      @racket[#:final (= y 20)] can continue with the second outer
      iteration, where the corresponding untyped loop stops after the
      first two elements.}
]
