#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/function
                     racket/sequence
                     racket/stream
                     racket/mutability
                     syntax/parse
                     (only-in srfi/43 vector-reverse!)
                     amb
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
@tech/refer{dynamic extent} of a search, which @racket[in-amb] and its
variants start.  The order in which a search finds its answers is
decided by the @tech{mergers} of its choices (@secref["mergers"]).

This is version 3 of the library.  It is not compatible with
version 2; @secref["v2"] lists the differences.


@section[#:tag "model"]{Evaluation Model}

A @deftech{search} runs a computation and reports a sequence of
@deftech{events}.  An event is an @deftech{answer} (the values that the
computation returned), a @deftech{pause}, or the end.  The computation
takes part in the search through three @deftech{effects}:

@itemlist[
@item{A @bold{failure} (@racket[amb-fail]) ends the computation here,
      without an answer.}
@item{A @bold{pause} (@racket[amb-pause]) makes the search report a
      pause.  The computation goes on when the search is asked for its
      next event.}
@item{A @bold{choice} (@racket[amb], @racket[for/amb],
      @racket[sequence->amb], @racket[make-amb]) runs the rest of the
      computation once for each @deftech{alternative} that its merger
      runs, and merges the events of these runs into one sequence.
      A choice among no alternatives, such as @racket[(amb)], fails.}
]

How the events of the alternatives are merged is the search strategy.
It belongs to the choice: every choice merges its alternatives with
the @tech{merger} that is current where the choice is made.  The four
mergers of @racketmodname[amb/merge] differ in when they pass the turn
from one alternative to the next:

@tabular[#:style 'boxed
         #:sep @hspace[2]
         #:row-properties '(bottom-border ())
(list (list @bold{merger} @bold{the turn passes on} @bold{strategy})
      (list @racket[amb-append] "when the first alternative has ended" "depth-first (the default)")
      (list @racket[amb-interleave] "at a pause" "interleaving, as in miniKanren")
      (list @racket[amb-alternate] "at every event, an answer too" "")
      (list @racket[amb-level] "every alternative runs up to its next pause" "breadth-first by pauses"))]

@amb-examples[
(define (count-from n)
  (amb n (begin (amb-pause) (count-from (add1 n)))))
(for/list ([x (in-amb (list (amb 'a 'b) (count-from 0)))]
           [_ 6])
  x)
(parameterize ([current-amb-merger amb-interleave])
  (for/list ([x (in-amb (list (amb 'a 'b) (count-from 0)))]
             [_ 6])
    x))
]

A search is lazy.  An alternative is not evaluated before it is
chosen, an alternative that a merger has not turned to has not started,
and after an event the search does nothing until it is asked for the
next one.


@section{Choices}

@defform[(amb expr ...)]{
John McCarthy's ambiguous operator: a choice among the
@racket[expr]s.  Its value is the value of one of them, and the rest
of the computation runs once for each alternative that the merger
runs.  Each @racket[expr] is evaluated only when chosen.

Each @racket[expr] is wrapped in a @racket[thunk], and the thunks are
collected into a mutable @tech/refer{vector}, on which
@racket[current-amb-shuffler] is called once.

@amb-examples[
(stream->list (in-amb (amb 1 2 3)))
(stream->list (in-amb (amb (amb 1 2 3) (amb 'a 'b 'c))))
]

An @tech{alternative} may return several @racket[values]:

@amb-examples[
(for/list ([(i j) (in-amb (amb (values 1 'x) (values 2 'y)))])
  (cons i j))
]

@racket[(amb)] is the choice among no alternatives, so it fails.  It
is commonly used to discard the computations that do not satisfy a
constraint.

@amb-examples[
(stream->list (in-amb (amb)))
(stream->list (in-amb (amb 1 2 (amb) 3 4)))
]

@racket[(amb)] is a choice like any other: the shuffler is called on
the empty vector, and the choice is made by @racket[make-amb], which
captures the continuation.  Its events are those of
@racket[(amb-fail)], but @racket[amb-fail] does less work; use it
where the cost of a failure matters, or where a continuation barrier
may be in the way.
}

@deftogether[(@defform*[((for/amb maybe-length (for-clause ...) break-clause ... body ...+)
                         (for/amb type-ann-maybe maybe-length (for-clause ...) type-ann-maybe expr ...+))]
              @defform*[((for*/amb maybe-length (for-clause ...) break-clause ... body ...+)
                         (for*/amb type-ann-maybe maybe-length (for-clause ...) type-ann-maybe expr ...+))])]{
Variants of @racket[for] and @racket[for*] in which each iteration of
the loop @racket[body] is an @tech{alternative}.  The @racket[body]
runs where the choice was made, as part of the search: a choice in it
is a choice of the same search.

@amb-examples[
(for/list ([x (in-amb (for/amb ([x '(1 2)]) (amb x (- x))))]) x)
]

@bold{Without @racket[#:length]}, the loop advances as the search
does: the clauses take a step when the merger of the choice turns to
the next alternative.  This works with @tech/refer{sequences} of any
size, infinite ones included.  The shuffler is not called.

@amb-examples[
(for/list ([x (in-amb (for/amb ([i (in-naturals)]) i))] [_ 5]) x)
(for/list ([x (in-amb (for/amb ([c "hello"]) c))]) x)
]

@bold{With @racket[#:length]}, the forms resemble @racket[for/vector]
and @racket[for*/vector].  Where the choice is made, the length
expression is evaluated and the clauses run until the loop finishes
or reaches that length.  Each loop @racket[body] is wrapped in a
@racket[thunk]; and at most @racket[_length] thunks are collected into
a mutable @tech/refer{vector}, on which @racket[current-amb-shuffler]
is called once.  The slots that no iteration fills are not
alternatives, unless @racket[#:fill] is given: then each of them is an
alternative with the value of the fill expression.

@amb-examples[
(for/list ([(a b) (in-amb (for/amb #:length 4 #:fill (values 0 'w)
                                   ([i #(1 2)] [j #(x y)])
                            (values i j)))])
  (cons a b))
(for/list ([(a b) (in-amb (for/amb #:length 4
                                   ([i #(1 2)] [j #(x y)])
                            (values i j)))])
  (cons a b))
]

With no iterations, @racket[for/amb] fails unless a positive length
and @racket[#:fill] supply alternatives.  A length of zero skips the
clauses and calls the shuffler on an empty vector.

The @racket[for-clause]s and @racket[break-clause]s are not part of
the search; see @secref["supply"].
}

@defproc[(sequence->amb [seq sequence?]) any]{
A choice among the elements of @racket[seq], which are produced as
the search goes on.  The sequence may be infinite.

@amb-examples[
(define (roll-doubles)
  (define a (sequence->amb (in-range 1 7)))
  (define b (sequence->amb (in-range 1 7)))
  (unless (= a b) (amb))
  (list a b))
(stream->list (in-amb (roll-doubles)))
]
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
(for/list ([x (in-amb (list (parameterize ([current-amb-merger amb-alternate])
                              (amb 1 2))
                            (amb 'a 'b)))])
  x)
]

See @secref["mergers"] for the mergers of @racketmodname[amb/merge],
and for the requirements on a merger of one's own.
}

@defparam[current-amb-shuffler shuffle! (-> mutable-vector? void?)]{
A @tech/refer{parameter} that holds a procedure to reorder the
@tech{alternatives} of a choice.  It is called once, where the choice
is made, on the vector of alternatives of every @racket[amb] and of
every @racket[for/amb] with @racket[#:length]; the vector may be
empty.  It is not called for the alternatives of a sequence.  The
default is @racket[void].

@amb-examples[
(eval:alts (require (only-in srfi/43 vector-reverse!)) (void))
(eval:alts
 (parameterize ([current-amb-shuffler vector-reverse!])
   (stream->list (in-amb (amb 1 2 3 4))))
 (parameterize ([current-amb-shuffler
                 (λ (v)
                   (define n (vector-length v))
                   (for ([i (quotient n 2)])
                     (define x (vector-ref v i))
                     (vector-set! v i (vector-ref v (- n i 1)))
                     (vector-set! v (- n i 1) x)))])
   (stream->list (in-amb (amb 1 2 3 4)))))
]

The shuffler permutes the alternatives of one choice.
@racket[(amb _a _b _c)] with a uniform shuffler chooses its first
alternative uniformly; the nested @racket[(amb (amb _a _b) _c)] does
not.

When the shuffler returns, the vector belongs to the choice.  The
choice does not keep a thunk once it has been taken: its slot is
cleared, and a long vector is replaced by a shorter copy of the rest
once half of it has been taken.  A shuffler should therefore not keep
the vector or look at it later.  It is also code that supplies
alternatives (@secref["supply"]).
}


@section{Observing a Search}

@defform[(in-amb expr maybe-handler)
         #:grammar ([maybe-handler (code:line)
                                   (code:line #:on-pause handler-expr)])]{
Starts a @tech{search} of @racket[expr] and returns its
@tech{answers} as a lazy @tech/refer{stream}.  Nothing of the search
runs before the first element is requested, and the stream keeps its
elements: reading them again does not run the search again.

In a @racket[for] clause, @racket[in-amb] is used directly as the
single-pass sequence of @racket[in-amb/do], without building the
stream.

The @tech{pauses} of the search are skipped, unless
@racket[handler-expr] is given.  It must produce a procedure of no
arguments, which is called once for each pause (@racket[_n] times for
a batch of @racket[_n]).  The values that it returns become one
element of the stream, in place of the pause; they do not go back into
the search.  The handler is called by whoever reads the stream,
outside the search.

@amb-examples[
(define s (in-amb (begin (amb-pause) (amb 1 2)) #:on-pause (λ () 'pause)))
(stream->list s)
(for/list ([x (in-amb (amb 1 (begin (amb-pause) 2) 3))]) x)
]
}

@defproc[(in-amb* [thk (-> any)] [handler (or/c #f (-> any)) #f]) stream?]{
The procedure form of @racket[in-amb]: the search of
@racket[(thk)].  The pause handler is an optional argument.
}

@defform[(in-amb/do expr maybe-handler)]{
Like @racket[in-amb], but returns a single-pass
@tech/refer{sequence} instead of a stream, so the answers are not kept.
Creating the sequence runs nothing of the search.  Iterating over it
again continues where the previous iteration stopped, and once it has
ended it stays empty.
}

@defproc[(in-amb*/do [thk (-> any)] [handler (or/c #f (-> any)) #f]) sequence?]{
The procedure form of @racket[in-amb/do].
}

@subsection[#:tag "exceptions"]{After an Exception}

What happens next depends on where the exception was raised.

@itemlist[
@item{@bold{In the pause handler.}  The pause is over.  The next read
      continues the search after that pause, and the handler is not
      called for it again.  This holds for the stream of
      @racket[in-amb] and for the sequence of @racket[in-amb/do]
      alike; the stream has no element for that pause.}
@item{@bold{In the computation of the search}, or when control leaves
      the computation through a continuation jump.  The search cannot
      go on, since the part that has run cannot be run again.  The
      next read, of the sequence or of the stream, raises
      @racket[exn:fail:contract] with the message
      @litchar{in-amb: the search cannot go on}.  The elements that a
      stream has already read stay in it.}
]

@amb-examples[
(define-values (more? get)
  (sequence-generate (in-amb/do (amb 1 (error "boom") 3))))
(get)
(eval:error (get))
(eval:error (get))
]

A pause handler cannot read the element that it is producing.  While
the handler of a pause runs, reading that element, from the stream or
from the sequence, raises @racket[exn:fail:contract] with the message
@litchar{in-amb: an element cannot be read by the handler of its own pause},
and nothing of the search runs.  This error is an exception in the
handler like any other: if the handler catches it and returns, the
values that it returns are the element; if it escapes from the
handler, the first case above applies.  The handler may read the
elements that the stream has read before.

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


@section[#:tag "local"]{Generators and Local Searches}

Programs that only use @racket[amb], @racket[for/amb] and
@racket[in-amb] never see the representation of a search.  This
section describes it, together with the operations that build choices
and searches from it.

@subsection[#:tag "protocol"]{The Generator Protocol}

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
@item{A positive integer @racket[_n] is @racket[_n] logical pauses,
      followed by the returned generator.  For @racket[(> _n 1)],
      the same events can be represented by a single pause, then
      @racket[(- _n 1)] pauses, then that generator.  When @racket[_n]
      is @racket[1], the single pause leads directly to the returned
      generator.}
]

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

@subsection{Choices from Generators}

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

@subsection{Searches as Generators}

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
@racket[amb-local] below, and others that the library does not
provide, such as the first answer of a search.
}

@defform[(amb-local expr)]{
Equivalent to @racket[(amb-local* (λ () expr))].
}

@defproc[(amb-local* [thk (-> any)]) any]{
An answer of @racket[(thk)], found by a search of its own: a
@deftech{local search}.  The inner mergers decide the order of its
events.  The local search runs on demand, supplying each answer as
an alternative of one choice in the enclosing search.  It is

@racketblock[
(make-amb (λ () (amb-generate* thk)))
]

The pauses of the local search are pauses of the search around it.

At the end of a computation, @racket[(amb-local expr)] has the same
logical events as @racket[expr] when the enclosing choice uses one of
the mergers of @racketmodname[amb/merge].  When the computation goes
on after it, the order can change: the enclosing merger schedules the
rest of the computation from the local search's answers.  If
@racket[amb-append] is used throughout, both forms have the same
logical events for any continuation.

@amb-examples[
(define (pause-then x) (amb-pause) x)
(parameterize ([current-amb-merger amb-level])
  (list (stream->list
         (in-amb (list (amb (pause-then 1) 2)
                       (amb 'x (pause-then 'y)))))
        (stream->list
         (in-amb (list (amb-local (amb (pause-then 1) 2))
                       (amb 'x (pause-then 'y)))))))
]
}

@subsection{The First Answer of a Search}

The library does not provide the first answer of a search, because
there are two ways to write it, and they differ in what happens to the
pauses of that search.

@racketblock[
(define (first-answer thk)
  (for/first ([v (in-amb* thk)]) v))
]

Here the pauses stay inside the search of @racket[(thk)].  For the
search around it, finding the answer is a single step, and no merger
gets a turn before it is done.  If @racket[(thk)] pauses forever, so
does the search around it, whatever its merger.  If there is no
answer, the result is @racket[#f], not a failure.

@racketblock[
(define (amb-once thk)
  (let loop ([gen (λ () (amb-generate* thk))])
    (define-values (v next) (gen))
    (cond
      [(list? v) (apply values v)]
      [(not v) (amb-fail)]
      [else (for ([_ v]) (amb-pause)) (loop next)])))
]

Here every pause of the search of @racket[(thk)] is passed on as a
pause of the search around it, which therefore stays fair, and no
answer is a failure.  This is the version that the committed choice of
a miniKanren needs.


@section[#:tag "mergers"]{Mergers}

@defmodule[amb/merge #:packages ("amb")]

A @deftech{merger} is a procedure that makes one @tech{generator} of
two; it is the strategy of a choice.  The merger of a choice is the
value of @racket[current-amb-merger] where the choice is made.

The four mergers of this module treat answers and the end of the
first generator alike,

@verbatim[#:indent 2]{
(a : s) ∨ t  =  a : (s ∨ t)        (amb-alternate: a : (t ∨ s))
end     ∨ t  =  t
}

and differ in what a pause of the first generator does.

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
Passes the turn at each event, at an answer too.
}

@defproc[(amb-level [s (-> any)] [t (-> any)]) (-> any)]{
Breadth-first search by pauses.  When @racket[s] pauses, @racket[t]
runs up to its own pause, and then the merged generator pauses once:
both wait together.  Among several alternatives this gives rounds: in
each round, every alternative runs up to its next pause.
}

@subsection{What a Merger Must Do}

Any procedure that takes two generators and returns a generator can be
the value of @racket[current-amb-merger].  The library requires three
things of it.

@itemlist[
@item{@bold{It follows the @seclink["protocol"]{generator protocol}.}
      Its results are lists, @racket[#f] or positive integers; it
      calls each generator at most once; and it does not call a
      generator after its end.}
@item{@bold{It preserves equivalence of event sequences.}  Replacing
      either input with one that produces the same logical events must
      preserve the merged events.  Compare answers in order and count
      individual pauses.  Reaching the end and diverging while
      producing the next event are distinct.  For example, an input
      that returns @racket[3] can be replaced with one that returns
      @racket[1] three times.  The library itself regroups batches of
      pauses.}
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

@subsection{Laws That a Merger May Satisfy}

These laws are optional.  All the mergers of this module satisfy the
first; @racket[amb-append] and @racket[amb-level] also satisfy the
second.  The examples below compare logical events, with a fixed
merger and the default shuffler for every choice.

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

@subsection[#:tag "fairness"]{Fairness Needs Pauses}

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

A choice among infinitely many alternatives is outside these
conditions.  Suppose that the supply produces one alternative at a
time without pauses, and that every alternative starts with a pause.
That first pause alone is not enough for every merger:

@itemlist[
@item{With @racket[amb-interleave], every scheduled remainder must also
      pause or end after a finite amount of host computation and
      finitely many answers.  Otherwise one alternative can keep
      producing answers without ever giving the others another turn.}
@item{With @racket[amb-alternate], every requested remainder must
      produce its next event or end after a finite amount of host
      computation.  Answers pass the turn too, so an infinite sequence
      of answers needs no further pauses.}
@item{With @racket[amb-level], this supply cannot finish the first
      round: before it returns the first merged pause, it would have
      to start infinitely many alternatives.  Not every infinite supply
      is ruled out: pauses in the supply itself can let a round finish
      after a finite part of it.}
]

The supply must also advance after a finite amount of host
computation, local observations must not hide the pauses needed for
scheduling, and all the choices involved must use the corresponding
fair mergers.  Under these conditions, @racket[amb-interleave] and
@racket[amb-alternate] reach each answer at a finite alternative index
and a finite event position.  The following search meets the
conditions for @racket[amb-interleave]:

@amb-examples[
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
alternatives: an effect of the search in it is an error.

@racket[(with-amb-mark expr)] is shorthand for
@racket[(call-with-amb-mark (λ () expr))].  Both return all the values
of @racket[expr] or @racket[(thk)], none included.

@racket[make-amb] marks its generator in this way.  A form that
collects its alternatives before it calls @racket[make-amb], as
@racket[for/amb] with @racket[#:length] does, should use one of these
for the code that collects them.  Without the mark, that code would be
part of the search, and a choice in it would run the rest of the
collection once for each alternative.

The mark can only refuse.  For the same control path to an effect,
adding it never turns a refused call into an allowed one, and it does
not start a search: the effects that it does not refuse still belong
to the search around it.

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


@section[#:tag "base"]{The Core}

@defmodule[amb/base #:packages ("amb") #:no-declare]

@racketmodname[amb/base] exports the seven bindings that the rest of
the library is built on, and nothing else: @racket[make-amb],
@racket[amb-generate*], @racket[amb-fail], @racket[amb-pause],
@racket[current-amb-merger], @racket[with-amb-mark] and
@racket[call-with-amb-mark].  @racketmodname[amb] exports them too.

A library that builds its own language on searches, such as a
miniKanren, needs nothing more: a disjunction is @racket[make-amb], a
failure is @racket[amb-fail], a relation pauses with
@racket[amb-pause], and running a goal is @racket[amb-generate*].  The
forms of @racketmodname[amb] are written for uniformity rather than
speed; a library that cares about the cost of each step can use these
bindings directly.


@section{Typed Interface}

@racketmodname[typed/amb], in the package @tt{typed-amb}, gives types
to the bindings of @racketmodname[amb]; @racketidfont{typed/amb/base}
and @racketidfont{typed/amb/merge} do the same for
@racketmodname[amb/base] and @racketmodname[amb/merge].

@subsection{Types}

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
@item{@racket[for/amb] and @racket[for*/amb] accept the type
      annotations of the second forms of their grammar above.}
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

@subsection{The Trusted Boundary}

The typed front end is written in @tt{typed/racket/base/shallow} and
imports the untyped implementations of this library through
@racketidfont{unsafe-require/typed/provide}.  It trusts them to
preserve the declared types, instead of adding run-time contracts to
their own checks of arguments and of the generator protocol.  Shallow
Typed Racket is kept so that the macros expand in every client: on
Racket 9.3, macros defined in Deep Typed Racket are rejected in Shallow
clients.  Clients may use Deep, Shallow or Optional Typed Racket.

This boundary is trusted; it does not protect against arbitrary
untyped code.  The protocol checks tell an answer from a pause or the
end, but they do not establish the types of the values in an answer.
An untyped merger, including one installed through the shared
@racket[current-amb-merger] parameter, must preserve the types of the
answers; if it makes up an answer of another type, the types of a
typed observer no longer hold.  The same holds for an untyped shuffler,
which must leave thunks of the right type in the vector.

On Racket 9.3, Deep @racket[require/typed] cannot generate a contract
for the variable-arity polymorphic @racketidfont{Merger}.  A custom
merger can be written in typed code, or imported through an explicitly
trusted @racketidfont{unsafe-require/typed}.  The standard mergers of
the library use such a trusted boundary and do not seal the answer
lists.

@subsection[#:tag "typed-limitations"]{Limitations on Racket 9.3}

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


@section[#:tag "v2"]{Changes from Version 2}

Version 3 is a new design.  A search is no longer a tree whose
frontier a driver schedules, but a merge of generators, and the
strategy is a value that belongs to each choice.  Programs that only
use @racket[amb], @racket[for/amb], @racket[amb-fail] and
@racket[in-amb] with the default strategy behave as before.

@bold{The most visible change.}  In version 2,
@racket[current-amb-fair?] made a search complete over infinitely many
alternatives without any change to the program.  In version 3,
fairness comes from the pauses of the program (@secref["fairness"]),
so such a program, moved to version 3 unchanged, does not terminate.
To port it, choose @racket[amb-interleave], and put an
@racket[(amb-pause)] in every alternative of an infinite choice and at
the entry of every recursive procedure that can recur forever without
an answer.

The differences, one by one:

@tabular[#:style 'boxed
         #:sep @hspace[2]
         #:row-properties '(bottom-border ())
         #:column-properties '(top top)
(list (list @bold{Version 2} @bold{Version 3})
      (list @para{@racket[current-amb-depth-first?] set to @racket[#f] gave breadth-first search by choices.}
            @para{The parameter is removed.  @racket[amb-level] is breadth-first by pauses; in a program without pauses it is depth-first.})
      (list @para{@racket[current-amb-fair?], set inside a computation, decided where the tree went back into the frontier of the whole search.}
            @para{The parameter is removed.  @racket[current-amb-merger] belongs to the choice made under it, and merges the alternatives of that choice only.})
      (list @para{The two parameters gave four strategies.}
            @para{There are four mergers, which are different strategies.  On programs without pauses, depth-first is @racket[amb-append], and depth-first with fairness is @racket[amb-alternate]; the two breadth-first strategies have no counterpart.})
      (list @para{@racket[(amb)] was answered by the handler of @racketidfont{#:empty-handler}, and abandoned the rest of the computation.  A pause was written @racket[(when (amb #t #f) (amb))].}
            @para{@racket[(amb)] fails.  A pause is @racket[(amb-pause)]; the computation goes on after it, and the observer sees it through @racket[#:on-pause].})
      (list @para{A choice with no alternatives, such as an empty @racket[for/amb], was answered by that handler too.}
            @para{It fails.})
      (list @para{The handler ran inside the search, where @racket[(amb)] was called.  Reaching @racket[(amb)] of the same search from the handler was a reentry error.}
            @para{The handler of @racket[#:on-pause] is called by the observer, outside the search.  An effect in it belongs to the search around the observer.})
      (list @para{The shuffler was not called for a choice with no alternatives.}
            @para{It is called once for every vector of alternatives, the empty one too.})
      (list @para{A fair search needed no pauses.}
            @para{It needs them; see above.})
      (list @para{@racket[make-amb] took two procedures, @racket[_more?] and @racket[_get].}
            @para{It takes one @tech{generator}.  @racket[sequence->amb] is exported.})
      (list @para{@racketidfont{amb/private/amb} exported @racketidfont{unsafe-amb*}.}
            @para{The module is removed.})
      (list @para{A choice or a failure in a clause of @racket[for/amb] did something that depended on the implementation.}
            @para{It is an error (@secref["supply"]).})
      (list @para{After an exception in the search, reading on ran the same part of the search again.}
            @para{Reading on is an error (@secref["exceptions"]).}))]
