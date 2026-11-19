#lang scribble/manual

@(require (for-label racket/base
                     racket/stream
                     amb
                     amb/base
                     amb/merge)
          "utils.rkt")

@title{amb: Ambiguous Operator}
@defmodule[amb       #:packages ("amb")]
@defmodule[typed/amb #:packages ("typed-amb") #:no-declare]
@author[@author+email["Noah Ma" "noahstorym@gmail.com"]]


The @racketmodname[amb] library provides John McCarthy's ambiguous
operator for nondeterministic programming in direct style.  A choice is
an ordinary expression: @racket[(amb 1 2 3)] returns one of its
alternatives, and the rest of the computation runs once for each of
them as the search demands it.  A @tech{search} reports the results of
those runs.

@amb-examples[
(for/list ([x (in-amb (let ([a (amb 1 2 3)]
                            [b (amb 10 20)])
                        (when (odd? (+ a b)) (amb))
                        (list a b)))])
  x)
]

Choices, and the other @tech{effects}, can only be used within the
@tech/refer{dynamic extent} of a search.  The library is organized in
three layers:

@itemlist[
@item{@racketmodname[amb] (@secref["surface"]) provides the everyday
      choice forms (@racket[amb], @racket[for/amb],
      @racket[sequence->amb], @racket[amb-local]) and observers
      (@racket[in-amb], @racket[in-amb/do]), and re-exports
      @racketmodname[amb/base].  @racketmodname[typed/amb]
      (@secref["typed"]) gives the same interface Typed Racket types.}
@item{@racketmodname[amb/base] (@secref["base"]) is the core on which
      everything else is built.  It defines the @tech{generator}
      protocol and the primitive effects: @racket[amb-generate*] turns
      a search into a generator of events (reifying it), and
      @racket[make-amb] turns a generator of alternatives into a choice
      (reflecting it), alongside @racket[amb-fail] and
      @racket[amb-pause].}
@item{@racketmodname[amb/merge] (@secref["mergers"]) provides the
      standard @tech{mergers}, which decide the search strategy of each
      choice.}
]

This is version 3 of the library.  It is not compatible with
version 2; @secref["v2"] lists the differences.

@table-of-contents[]

@include-section["model.scrbl"]
@include-section["surface.scrbl"]
@include-section["core.scrbl"]
@include-section["mergers.scrbl"]
@include-section["typed.scrbl"]
@include-section["v2.scrbl"]
