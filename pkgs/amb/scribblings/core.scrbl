#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/math
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
@tech{generators}: @racket[(λ () (amb-generate* _thk))] reifies a
search into a generator of events, and @racket[make-amb] reflects a
generator of alternatives back into a choice in direct style.  A
library that builds its own language on searches, such as a
miniKanren, needs nothing more: a disjunction is @racket[make-amb], a
failure is @racket[amb-fail], a relation pauses with
@racket[amb-pause], and running a goal is @racket[amb-generate*].  The
surface forms of @racketmodname[amb] (@secref["surface"]) are written
for uniformity rather than speed; a library that cares about the cost
of each step can use these bindings directly.


@section[#:tag "protocol"]{The Generator Protocol}

A search is represented by a @deftech{generator}: a thunk that returns
two values—a @deftech{result} for the next step and a generator for
the rest of the search.

@racketgrammar*[
#:literals (-> values list? exact-positive-integer?)
[generator (-> (values result generator))]
[result list? #f exact-positive-integer?]]

@itemlist[
@item{A @bold{list} is an answer holding its return values.  The empty
      list @racket['()] represents a zero-valued answer, not the end.}
@item{@racket[#f] signals the end of the search.  The companion
      generator returned alongside @racket[#f] must not be called.}
@item{A positive integer @racket[_n] represents a batch of @racket[_n]
      @deftech{logical pauses}, followed by the returned generator.
      When @racket[(> _n 1)], this is equivalent to one pause followed
      by @racket[(- _n 1)] pauses and then that generator; when
      @racket[_n] is @racket[1], the pause leads directly to the
      returned generator.}
]

Expanding each batch @racket[_n] into @racket[_n] individual pauses
yields a generator's sequence of @deftech{logical events}.  Throughout
this manual, two generators are considered equivalent when they produce
the same logical events (and diverge in the same places), regardless
of how pauses are batched.

@racket[0] is not a valid result.  If a generator returns an invalid
first value (not a list, @racket[#f], or a positive integer), the
consumer raises @racket[exn:fail:contract].  The second value is also
part of the protocol, but is not checked unless it is called.

Because @racket[sequence-generate*] already returns either a list of
values or @racket[#f] together with the next step,
@racket[(λ () (sequence-generate* _seq))] is a pause-free generator
for any @tech/refer{sequence}.

Generators are @bold{single-pass}: every generator—and each generator
it returns—must be called at most once.  The behavior of calling a
generator more than once is unspecified (for example, the library's
vector-backed choice generators raise @racket[exn:fail:contract]).

Generators appear in three places: @racket[make-amb] consumes a
generator of alternatives, @racket[amb-generate*] exposes a search as
a generator, and a @tech{merger} combines two generators into one.


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

For two alternatives @racket[_m], @racket[_n] supplied without pauses,
the choice merges them from the right by the merger @racket[_μ] that is
current where the choice is made, merging the last one with the end of
the supply:

@racketblock[
(_μ _kₘ (_μ _kₙ _end))
]

where @racket[_kₘ] is the generator of the rest of the computation
with the alternative @racket[_m].  The inner merge is not made before
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

@defproc[(amb-pause [n natural? 1]) void?]{
With a positive @racket[n], suspends the current branch and reports a
batch of @racket[n] logical pauses.  When the branch resumes after the
batch, @racket[amb-pause] returns @racket[(void)].

With @racket[0], returns @racket[(void)] immediately, without
suspending or reporting an event.  This also works outside a search
and during the supply of alternatives.

A pause is where a @tech{merger} may pass the turn to another
alternative, including between logical pauses in a batch.  The
paused branch is resumed once after the batch.  For example, a
@racket[dynamic-wind] around the call is exited once and re-entered
when the branch resumes.  With the default merger,
@racket[amb-append], the paused branch keeps the turn.
See @secref["fairness"].

@amb-examples[
(stream->list (in-amb (begin (amb-pause) 'x)))
(stream->list (in-amb (begin (amb-pause 3) 'x) #:on-pause (λ () 'pause)))
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
@item{@bold{On-demand supply}: the @tech{generator} given to
      @racket[make-amb], the @tech/refer{sequence} given to
      @racket[sequence->amb], and the @racket[_for-clause]s and
      @racket[_break-clause]s of @racket[for/amb] and @racket[for*/amb]
      without @racket[#:length].}
@item{@bold{Eager pre-collection and shuffling}: the @racket[#:length]
      expression and clauses/guards of @racket[for/amb] and
      @racket[for*/amb] with @racket[#:length], and the procedure of
      @racket[current-amb-shuffler].}
]

On-demand supply runs after @racket[make-amb] leaves the search, when
the merger turns to the next alternative—which may be long after the
choice was made, and in another dynamic context.  Eager pre-collection
and shuffling run at the choice point before @racket[make-amb] is
called, under a supply mark (see @secref["host-code"]).  In both
cases, an effect of the enclosing search has no meaning here and
raises an error.

@amb-examples[
(eval:error
 (stream->list (in-amb (for/amb ([x '(1 2 3)] #:when (amb #t #f)) x))))
]

The @racket[_body] of @racket[for/amb], and the expressions of
@racket[amb], are part of the search.

@amb-examples[
(stream->list (in-amb (for/amb ([x '(1 2 3)]) (if (amb #t #f) x (- x)))))
]

An independent search started inside supply code is unaffected.

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
Marks @racket[expr] (or @racket[(thk)]) as code that supplies
alternatives, so that any effect aimed at the enclosing search raises
@racket[exn:fail:contract].  @racket[(with-amb-mark expr)] is
shorthand for @racket[(call-with-amb-mark (λ () expr))], and both
return all values produced by @racket[expr] or @racket[(thk)],
including zero values.

@racket[make-amb] already marks its generator automatically.  You need
@racket[with-amb-mark] when writing a custom choice form that gathers
its alternatives @emph{before} calling @racket[make-amb]—for example,
collecting thunks into a list or vector, just as @racket[for/amb] with
@racket[#:length] does.  Without the mark, a stray @racket[amb] inside
that collection loop would run as part of the enclosing search and
silently duplicate the rest of the collection for each branch instead
of failing fast.

The mark only blocks effects targeting the enclosing search: it does
not start a search of its own, and an independent nested search
started inside @racket[expr] (via @racket[in-amb] or
@racket[amb-generate*]) runs normally.  In the example below,
@racket[with-amb-mark] catches the @racket[(amb #t #f)] inside the
@racket[for/list] that builds the thunk list for @racket[choose-among].

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

See @secref["host-code"] for the full rules on where host code runs
relative to search boundaries.
