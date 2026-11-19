#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/sequence
                     racket/stream
                     amb
                     amb/base
                     amb/merge)
          "utils.rkt")

@title[#:tag "base"]{The Core: Generators and Effects}

@defmodule[amb/base #:packages ("amb") #:no-declare]
@declare-exporting[amb/base amb]

@racketmodname[amb/base] exports the seven bindings that the rest of
the library is built on: @racket[make-amb], @racket[amb-generate*],
@racket[amb-fail], @racket[amb-pause], @racket[current-amb-merger],
@racket[with-amb-mark] and @racket[call-with-amb-mark].
@racketmodname[amb] re-exports all of them.

The design centers on the duality between direct-style searches and
@tech{generators}: @racket[amb-generate*] reifies a search into a
generator of events, and @racket[make-amb] reflects a generator of
alternatives back into a choice in direct style.  A library that builds
its own language on searches, such as a miniKanren, needs nothing more:
a disjunction is @racket[make-amb], a failure is @racket[amb-fail], a
relation pauses with @racket[amb-pause], and running a goal is
@racket[amb-generate*].  The surface forms of @racketmodname[amb]
(@secref["surface"]) are written for uniformity rather than speed; a
library that cares about the cost of each step can use these bindings
directly.


@section[#:tag "protocol"]{The Generator Protocol}

A search is represented by a @deftech{generator}: a procedure of no
arguments that returns two values, a @deftech{result} and the
generator of what comes after it.

@racketgrammar*[
#:literals (list? exact-positive-integer?)
[generator (-> (values result generator))]
[result list? #f exact-positive-integer?]]

@itemlist[
@item{A @bold{list} is an answer: the list of its values.  The empty
      list is an answer of no values, not the end.}
@item{@racket[#f] is the end.  The generator returned with it must
      not be called.}
@item{A positive integer @racket[_n] is a batch of @racket[_n]
      @deftech{logical pauses}, followed by the returned generator.
      For @racket[(> _n 1)], the same events can be represented by a
      single pause, then @racket[(- _n 1)] pauses, then that generator.
      When @racket[_n] is @racket[1], the single pause leads directly
      to the returned generator.}
]

Expanding each batch @racket[_n] into @racket[_n] individual pauses
gives a generator's sequence of @deftech{logical events}.  Throughout
this manual, two generators are considered equivalent when they produce
the same logical events (and diverge in the same places), even if they
group pauses into different batches.

@racket[0] is not a result.  A generator that returns anything else
is in error, and whoever reads the result raises
@racket[exn:fail:contract].

A list or @racket[#f] is what @racket[sequence-generate*] returns, so
@racket[(λ () (sequence-generate* _seq))] is a generator without
pauses.

A generator is @bold{single-pass}: it, and each generator that it
returns, is called at most once.  Calling one twice would run again a
part of the computation that has already run.

Generators appear in three places: @racket[make-amb] takes the
generator that supplies the alternatives of a choice,
@racket[amb-generate*] returns the generator of a search, and a
@tech{merger} makes one generator of two.


@section{Core Primitives}

@defproc[(amb-generate* [thk (-> any)]) (values (or/c list? #f exact-positive-integer?) (-> any))]{
Runs @racket[(thk)] as a @tech{search} of its own, up to its first
@tech{event}, and returns that event as a @tech{result} together with
the @tech{generator} of the rest, in the manner of
@racket[sequence-generate*].  No effect of @racket[(thk)] reaches a
search around it, and nothing more of the search runs before the
generator is called.

@racket[amb-generate*] is eager: it runs the search up to its first
event when it is called.  @racket[(λ () (amb-generate* thk))] is the
generator of the whole search; it is what @racket[in-amb*/do] reads.

@amb-examples[
(define-values (v next) (amb-generate* (λ () (amb 1 (begin (amb-pause) 2)))))
v
(define-values (v2 next2) (next))
v2
(define-values (v3 next3) (next2))
v3
]

Together with @racket[make-amb], it builds searches inside a search:
@racket[amb-local] (@secref["local"]), and others that the library does
not provide, such as the first answer of a search.
}

@defproc[(make-amb [next (-> any)]) any]{
The primitive that all other choices are built on: a choice among the
alternatives that the @tech{generator} @racket[next] supplies.  Each
answer of @racket[next] is the values of one alternative, which
@racket[make-amb] returns to its continuation.  The pauses of
@racket[next] are pauses of the choice, at the same place.

For example, @racket[(sequence->amb _seq)] is
@racket[(make-amb (λ () (sequence-generate* _seq)))].

@amb-examples[
(define (countdown n)
  (λ ()
    (if (zero? n)
        (values #f void)
        (values (list n) (countdown (sub1 n))))))
(stream->list (in-amb (make-amb (countdown 3))))
]

The alternatives @racket[_a], @racket[_b] are merged from the right by
the merger @racket[_μ] that is current where the choice is made, and
the last one is merged with the end of the supply:

@racketblock[
(_μ _K_a (_μ _K_b _end))
]

where @racket[_K_a] is the generator of the rest of the computation
with the alternative @racket[_a].  The inner merge is not made before
the outer one turns to it, so @racket[next] is not asked for an
alternative before its turn.  @racket[make-amb] keeps the merge with
the end whatever the merger is; for a merger that has the end as a
right unit, such as those of @racketmodname[amb/merge], it makes no
difference.

@racket[next] is code that supplies alternatives: it must not choose,
fail or pause in the search that it supplies (@secref["supply"]).  It
pauses by returning a number.
}

@defproc[(amb-fail) none/c]{
Ends the computation here, without an answer.

@amb-examples[
(stream->list (in-amb (amb 1 (amb-fail) 2)))
]

Since nothing of the computation will be resumed, @racket[amb-fail]
does not capture its continuation.  A continuation barrier between it
and its search, such as the barrier around an exception handler that
@racket[raise] calls, does not stop it.  A choice or a pause captures
its continuation, and a barrier stops it.  This holds for a choice
among no alternatives too, such as @racket[(amb)], an empty
@racket[for/amb], or @racket[(sequence->amb '())]: it fails, but it is
still a choice.
}

@defproc[(amb-pause) void?]{
Makes the search report a pause.  The computation goes on from here
when the search is asked for its next event, and @racket[amb-pause]
then returns @racket[(void)].

A pause is where a @tech{merger} may pass the turn to another
alternative.  With the default merger, @racket[amb-append], a pause
changes nothing but the events seen by an observer with a pause
handler.  See @secref["fairness"].

@amb-examples[
(stream->list (in-amb (begin (amb-pause) 'x)))
(stream->list (in-amb (begin (amb-pause) 'x) #:on-pause (λ () 'pause)))
]
}

@defparam[current-amb-merger merge (-> (-> any) (-> any) (-> any))]{
A @tech/refer{parameter} that holds the @tech{merger} of the choices
made while it is current.  The default is @racket[amb-append].

The merger is read where a choice is made, so different choices of
one search can use different mergers.

@amb-examples[
(require amb/merge)
(for/list ([x (in-amb (list (parameterize ([current-amb-merger amb-alternate])
                              (amb 1 2))
                            (amb 'a 'b)))])
  x)
]

See @secref["mergers"] for the mergers of @racketmodname[amb/merge],
and for the requirements on a merger of one's own.
}


@section[#:tag "supply"]{Code That Supplies Alternatives}

Some code runs for a choice without being part of the search:

@itemlist[
@item{the @tech{generator} given to @racket[make-amb], and the sequence
      given to @racket[sequence->amb];}
@item{the @racket[_for-clause]s and @racket[_break-clause]s of
      @racket[for/amb] and @racket[for*/amb], and their
      @racket[#:length] expression;}
@item{the procedure of @racket[current-amb-shuffler].}
]

This code runs when the merger of the choice turns to the next
alternative, which may be long after the choice was made, and in
another dynamic context.  An effect there has no meaning for the
search, and is an error:

@amb-examples[
(eval:error
 (stream->list (in-amb (for/amb ([x '(1 2 3)] #:when (amb #t #f)) x))))
]

The @racket[_body] of @racket[for/amb], and the expressions of
@racket[amb], are part of the search:

@amb-examples[
(stream->list (in-amb (for/amb ([x '(1 2 3)]) (if (amb #t #f) x (- x)))))
]

A search of its own is not concerned, wherever it runs:

@amb-examples[
(stream->list
 (in-amb (for/amb ([x (in-amb (amb 1 2))]) (* 10 x))))
]

With @racket[#:length], clauses up to the length limit or the end of
the loop run where the choice is made.  An effect in those clauses
raises an error there, even if only the first answer is requested.
Without @racket[#:length], the error is raised when the search reaches
the iteration that has the effect.

@deftogether[(@defform[(with-amb-mark expr)]
              @defproc[(call-with-amb-mark [thk (-> any)]) any])]{
Evaluates @racket[expr], or calls @racket[thk], as code that supplies
alternatives: an effect of the enclosing search in it is an error.

@racket[(with-amb-mark expr)] is shorthand for
@racket[(call-with-amb-mark (λ () expr))].  Both return all the values
of @racket[expr] or @racket[(thk)], none included.

@racket[make-amb] marks its generator in this way.  A form that
collects its alternatives before it calls @racket[make-amb], as
@racket[for/amb] with @racket[#:length] does, should use one of these
for the code that collects them.  Without the mark, that code would be
part of the search, and a choice in it would run the rest of the
collection once for each alternative.

The mark only refuses effects aimed at the enclosing search: it does
not start a search of its own, and it does not interfere with a nested
search started inside it (such as an @racket[in-amb] or
@racket[amb-generate*] inside @racket[expr]).  Adding a mark never
makes a refused effect valid.

@amb-examples[
(define (choose-among . thunks)
  ((sequence->amb thunks)))
(eval:error
 (stream->list
  (in-amb (apply choose-among
                 (with-amb-mark
                  (for/list ([i 3] #:when (amb #t #f))
                    (λ () i)))))))
]
}

@subsection{Where Host Code Runs}

The boundary of a search protects the body of the search and the
continuations that it captures.  An effect leaves its search before
any code that interprets the choice runs, and only the continuation of
the effect, when it is resumed, is inside a search again.  So:

@itemlist[
@item{The body of a search, and every resumed continuation, run inside
      the search.}
@item{The supply of a choice, and its merger from the first call on,
      run in the dynamic context of whoever calls them: first the
      caller of the search, later whoever asks for the next event.
      They follow the same rule, but need not see the same context.  An
      effect in a merger belongs to the search around that caller, if
      there is one.}
@item{The handler of @racket[#:on-pause] runs outside the search that
      it observes.  An effect in it belongs to the search around the
      observer, if there is one.}
@item{Outside any search, an effect raises
      @racket[exn:fail:contract].  With a supply mark active it reports
      the supply-phase error.  Otherwise its message ends in
      @litchar{not in a search}.}
]
