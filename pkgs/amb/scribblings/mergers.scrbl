#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/stream
                     amb
                     amb/base
                     amb/merge)
          "utils.rkt")

@title[#:tag "mergers"]{Mergers}

@defmodule[amb/merge #:packages ("amb")]

A @deftech{merger} is a procedure that makes one @tech{generator} of
two; it is the strategy of a choice.  The merger of a choice is the
value of @racket[current-amb-merger] where the choice is made.

All four mergers of this module agree on the end of the first
generator (@racketidfont{end ∨ t = t}), and all except
@racket[amb-alternate] agree on answers too:

@verbatim[#:indent 2]{
end     ∨ t  =  t
(a : s) ∨ t  =  a : (s ∨ t)        (amb-alternate: a : (t ∨ s))
}

They differ in what a pause of the first generator does:

@verbatim[#:indent 2]{
amb-append       (pause s) ∨ t          =  pause (s ∨ t)    s keeps the turn
amb-interleave   (pause s) ∨ t          =  pause (t ∨ s)    the turn passes on
amb-alternate    (pause s) ∨ t          =  pause (t ∨ s)    the turn passes on
amb-level        (pause s) ∨ (pause t)  =  pause (s ∨ t)    both wait together
}

@defproc[(amb-append [s (-> any)] [t (-> any)]) (-> any)]{
All the events of @racket[s], then those of @racket[t]: depth-first
search.  A pause of @racket[s] is a pause of the merged generator, and
@racket[s] keeps the turn.  If @racket[s] never ends, @racket[t] never
runs.
}

@defproc[(amb-interleave [s (-> any)] [t (-> any)]) (-> any)]{
Passes the turn at each pause: when @racket[s] pauses, the merged
generator pauses and continues with @racket[t].  This is the
interleaving of miniKanren.
}

@defproc[(amb-alternate [s (-> any)] [t (-> any)]) (-> any)]{
Passes the turn at each event, answers included.
}

@defproc[(amb-level [s (-> any)] [t (-> any)]) (-> any)]{
Breadth-first search by pauses.  When @racket[s] pauses, @racket[t]
runs up to its own pause, and then the merged generator pauses once:
both wait together.  Among several alternatives this gives rounds: in
each round, every alternative runs up to its next pause.
}


@section{What a Merger Must Do}

Any procedure that takes two generators and returns a generator can be
the value of @racket[current-amb-merger].  The library requires three
things of it.

@itemlist[
@item{@bold{It follows the @seclink["protocol"]{generator protocol}.}
      Its results are lists, @racket[#f] or positive integers; it
      calls each generator at most once; and it does not call a
      generator after its end.}
@item{@bold{It preserves equivalence of @tech{logical events}.}
      Replacing either input with one that produces the same logical
      events must preserve the merged events.  Compare answers in
      order and count individual pauses.  Reaching the end and
      diverging while producing the next event are distinct.  For
      example, an input that returns @racket[3] can be replaced with
      one that returns @racket[1] three times.  The library itself
      regroups batches of pauses.}
@item{@bold{It has no effects of a search.}  A merger must not call
      @racket[amb], @racket[amb-pause], @racket[amb-fail] or any other
      choice.  A merger runs outside the search whose alternatives it
      merges, from its first call on: first it is called by the caller
      of that search, later by whoever asks the merged generator for an
      event.  Such an effect therefore never belongs to the search that
      the merger works for, but to whatever search surrounds its caller,
      if any, and the merger cannot know which.  A merger pauses by
      returning a number.}
]

Here is @racket[amb-interleave] written out.  It shows how a merger
handles a batch of @racket[_n] pauses: it takes one pause of the
batch, passes the turn, and leaves the other @racket[(- _n 1)] in
front of @racket[_s].

@racketblock[
(define ((my-interleave s t))
  (let-values ([(v s) (s)])
    (cond
      [(list? v) (values v (my-interleave s t))]
      [(not v) (t)]
      [else (values 1 (my-interleave t (pauses (sub1 v) s)))])))

(code:comment "n pauses, then g")
(define (pauses n g)
  (if (zero? n)
      g
      (λ () (values 1 (pauses (sub1 n) g)))))
]

A version that returns @racket[(values v (my-interleave t s))] for a
pause, passing the turn once for a whole batch, is wrong: it gives
different events for @racket[3] than for @racket[1], @racket[1],
@racket[1].


@section{Laws That a Merger May Satisfy}

These laws are optional.  All the mergers of this module satisfy the
first; @racket[amb-append] and @racket[amb-level] also satisfy the
second.  The examples below compare @tech{logical events}, with a
fixed merger and the default shuffler for every choice.

@itemlist[
@item{@bold{The end is a unit.}  Merging with a generator that ends at
      once changes no event.  @racket[make-amb] merges the last
      alternative with the end of the supply; under this law that
      merge is invisible, so @racket[(amb _e)] has the events of
      @racket[_e], and an alternative that fails immediately can be
      added or removed without changing the events.}
@item{@bold{Associativity.}  Nested choices have the events of one
      flat choice: @racket[(amb (amb _a _b) _c)] and
      @racket[(amb _a (amb _b _c))] are the same.  This does not hold
      for @racket[amb-interleave] and @racket[amb-alternate], where
      nesting decides how often each alternative gets the turn.}
]

An empty choice ends the branch without invoking the merger.
@racket[(amb (amb) (amb))] still merges its two alternatives.  A
merger without the unit law can make that difference visible.

@amb-examples[
(require amb/merge)
(define ((pause-before-append s t))
  (values 1 (amb-append s t)))
(parameterize ([current-amb-merger pause-before-append])
  (list (stream->list (in-amb (amb) #:on-pause (λ () 'pause)))
        (stream->list (in-amb (amb (amb) (amb)) #:on-pause (λ () 'pause)))))
]

Whatever the merger, the rest of the computation distributes over the
alternatives of a choice: @racket[(f (amb _a _b))] has the events of
@racket[(amb (f _a) (f _b))].  This needs no law.

A merger that gives every alternative the turn again and again is
@emph{fair}.  Fairness is a property of whole runs, not an equation.
@racket[amb-append] is not fair.


@section[#:tag "fairness"]{Fairness Needs Pauses}

A merger can only pass the turn at an event.  A computation that runs
forever without an answer and without a pause keeps the turn, whatever
the merger.  A search with @racket[amb-interleave],
@racket[amb-alternate] or @racket[amb-level] therefore finds every
answer at a finite depth, provided that

@itemlist[
@item{every choice has finitely many alternatives (but see below);}
@item{every step of the computation, and of the supply of
      alternatives, terminates; and}
@item{every infinite path of the computation has infinitely many
      pauses.}
]

The usual way to meet the last condition is to pause at the entry of
every recursive procedure, as miniKanren does at every relation.  The
conditions concern all the choices and paths of the search: mixing in
an unfair merger, or hiding the pauses inside a local observation,
loses the guarantee.

A choice among infinitely many alternatives goes beyond those
conditions.  Suppose the supply produces one alternative at a time
without pauses, each step of the supply terminates, and every
alternative starts with a pause.  That opening pause alone is not
enough for every merger:

@itemlist[
@item{With @racket[amb-interleave], each alternative's continuation
      must also pause or end after finitely many answers and finite
      ordinary computation.  Otherwise one alternative can keep
      producing answers without ever giving the later alternatives
      their turn.}
@item{With @racket[amb-alternate], each alternative's continuation
      must produce its next event or end after finite ordinary
      computation.  Because answers pass the turn too, an alternative
      that produces infinitely many answers needs no further pauses.}
@item{With @racket[amb-level], a pause-free infinite supply cannot
      finish the first round: before returning the first merged pause,
      it would have to start infinitely many alternatives.  Pauses in
      the supply itself can still let each round finish after a finite
      prefix of alternatives.}
]

As before, local observations must not hide the pauses needed for
scheduling, and every choice involved must use the corresponding fair
merger.  Under these conditions, @racket[amb-interleave] and
@racket[amb-alternate] reach every answer that lies at a finite
position in the supply and after finitely many events.  The following
search meets these conditions for @racket[amb-interleave].

@amb-examples[
(require amb/merge)
(define (pythagorean)
  (define a (for/amb ([n (in-naturals 1)]) (amb-pause) n))
  (define b (for/amb ([n (in-naturals 1)]) (amb-pause) n))
  (define c (for/amb ([n (in-naturals 1)]) (amb-pause) n))
  (unless (= (+ (* a a) (* b b)) (* c c)) (amb))
  (list a b c))
(parameterize ([current-amb-merger amb-interleave])
  (stream-first (in-amb (pythagorean))))
]

Without the pauses, or with @racket[amb-append], this search does not
terminate.
