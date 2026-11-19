#lang scribble/manual

@(require (for-label racket/base
                     racket/sequence
                     racket/stream
                     amb
                     amb/base
                     amb/merge)
          "utils.rkt")

@title[#:tag "boundaries"]{Search Boundaries and Exceptions}

This section specifies the dynamic boundary of a @tech{search}: which
code runs inside the search versus in host context, and how the
observers (@racket[in-amb], @racket[in-amb/do]) behave when an
exception or reentrant read occurs.


@section[#:tag "host-code"]{Where Host Code Runs}

The boundary of a search protects the body of the search and the
continuations that it captures.  An @tech{effect} leaves its search
before any code that interprets the choice runs, and only the
continuation of the effect, when it is resumed, is inside a search
again.  So:

@itemlist[
@item{The body of a search, and every resumed continuation, run inside
      the search.}
@item{Eager pre-collection and shuffling (@racket[for/amb] with
      @racket[#:length], its length expression and clauses/guards, and
      @racket[current-amb-shuffler]) run at the choice point
      @emph{before} @racket[make-amb] leaves the search, under a supply
      mark (@secref["supply"]).  They therefore see the search body's
      dynamic parameters, while effects aimed at the enclosing search
      are rejected.}
@item{On-demand supply (the @tech{generator} of @racket[make-amb], the
      @tech/refer{sequence} of @racket[sequence->amb], and
      @racket[for/amb] clauses without @racket[#:length]), and a
      choice's @tech{merger} from its first call on, run @emph{after}
      leaving the search, in the @tech/refer{dynamic extent} of
      whoever calls them: first the caller of the search, later whoever
      asks for the next event.  They follow the same rule, but need not
      see the same context.  An effect in a merger belongs to the
      search around that caller, if there is one.}
@item{The handler of @racket[#:on-pause] runs outside the search that
      it observes.  An effect in it belongs to the search around the
      observer, if there is one.}
@item{Outside any search, an effect raises
      @racket[exn:fail:contract].  With a supply mark active it
      reports the supply-phase error.  Otherwise its message ends in
      @litchar{not in a search}.}
]


@section[#:tag "exceptions"]{After an Exception}

When an exception escapes while reading from @racket[in-amb] or
@racket[in-amb/do], what happens next depends on where the exception
was raised.

@itemlist[
@item{@bold{In the pause handler.}  The pause is over.  The next read
      continues the search after that pause, and the handler is not
      called for it again.  This holds for the @tech/refer{stream} of
      @racket[in-amb] and for the @tech/refer{sequence} of
      @racket[in-amb/do] alike; the @tech/refer{stream} has no element
      for that pause.}
@item{@bold{In the computation of the search}, or when control leaves
      the computation through a continuation jump.  The search cannot
      go on, since the part that has run cannot be run again.  The
      next read, of the @tech/refer{sequence} or of the
      @tech/refer{stream}, raises @racket[exn:fail:contract] with the
      message @litchar{in-amb: the search cannot go on}.  The elements
      that a @tech/refer{stream} has already read stay in it.}
]

@amb-examples[
(define-values (more? get)
  (sequence-generate (in-amb/do (amb 1 (error "boom") 3))))
(get)
(eval:error (get))
(eval:error (get))
]

A pause handler cannot read the element that it is producing.  While
the handler of a pause runs, reading that element, from the
@tech/refer{stream} or from the @tech/refer{sequence}, raises
@racket[exn:fail:contract] with the message
@litchar{in-amb: an element cannot be read by the handler of its own pause},
and nothing of the search runs.  This error is an exception in the
handler like any other: if the handler catches it and returns, the
values that it returns are the element; if it escapes from the
handler, the first case above applies.  The handler may read the
elements that the @tech/refer{stream} has read before.

The observers are not thread-safe, and their guarantees stop here:

@itemlist[
@item{If another thread reads an element while its pause handler is
      running, it gets the reentrancy error described above.}
@item{If the thread of a handler is killed while the handler runs, the
      cleanup on exit does not run.  The element stays in the running
      state, and every later read raises the same reentrancy error.}
@item{Resuming the continuation of a handler several times must not be
      used to consume the single-pass state of a search more than
      once.}
]
