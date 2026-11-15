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
operator for nondeterministic programming, in direct style: a choice
is an ordinary expression, and the rest of the computation runs once
for each alternative that is chosen.

All uses of @racket[amb] and of the other @tech{effects} must occur
within the @tech/refer{dynamic extent} of a @tech{search}, which
@racket[in-amb] and its variants start.  The order in which a search
finds its answers is decided by the @tech{mergers} of its choices (see
@secref["mergers"]).

@amb-examples[
(for/list ([x (in-amb (let ([a (amb 1 2 3)]
                            [b (amb 10 20)])
                        (when (odd? (+ a b)) (amb))
                        (list a b)))])
  x)
]

This is version 3 of the library.  It is not compatible with version
2; @secref["v2"] lists the differences.


@section[#:tag "model"]{Evaluation Model}

A @deftech{search} runs a computation, and reports a sequence of
@deftech{events}: an @deftech{answer} (the values that the computation
returned), a @deftech{pause}, or the end.  The computation takes part
in the search through three @deftech{effects}:

@itemlist[
@item{@bold{failure} (@racket[amb-fail], @racket[(amb)]): the
      computation ends here, with no answer;}
@item{a @bold{pause} (@racket[amb-pause]): the search reports a pause,
      and the computation goes on when the search is asked for its
      next event;}
@item{a @bold{choice} (@racket[amb], @racket[for/amb],
      @racket[sequence->amb], @racket[make-amb]): the rest of the
      computation runs once for each @deftech{alternative}, and the
      events of these runs are merged into one sequence.}
]

How the events of the alternatives are merged is the search strategy,
and it belongs to the choice: a choice merges its alternatives with
the @tech{merger} that is current where the choice is made.  The
default merger, @racket[amb-append], reports all the events of the
first alternative, then all those of the second, and so on: this is
depth-first search, or chronological backtracking.  Other mergers pass
the turn on to the next alternative at a pause
(@racket[amb-interleave]), at every event (@racket[amb-alternate]), or
let all the alternatives run up to their next pause before they report
one (@racket[amb-level]).

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

Nothing runs before it is asked for.  An alternative is not evaluated
before it is chosen; an alternative that a merger has not turned to
has not started; and after an event, a search does nothing until it is
asked for the next one.

@subsection[#:tag "protocol"]{The Generator Protocol}

A search is represented by a @deftech{generator}: a procedure of no
arguments that returns two values, a @deftech{result} and the
generator of what comes after it.

@racketgrammar*[
#:literals (list? exact-positive-integer?)
[generator (-> (values result generator))]
[result list? #f exact-positive-integer?]]

@itemlist[
@item{A @bold{list} is an answer: the values of one element.  The empty
      list is an answer of no values, not the end.}
@item{@racket[#f] is the end.  The generator that is returned with it
      must not be called.}
@item{A positive integer @racket[_n] is @racket[_n] pauses.  It means
      exactly the same as a pause, followed by a generator that
      returns @racket[(- _n 1)] pauses: how pauses are written in
      batches is not information, and no code may depend on it.}
]

@racket[0] is not a result.  A generator that returns anything else is
in error, and whoever reads the result raises
@racket[exn:fail:contract].

A list or @racket[#f] is what @racket[sequence-generate*] returns, so
@racket[(λ () (sequence-generate* _seq))] is a generator, one without
pauses.

A generator is @bold{single-pass}: it, and each generator that it
returns, is called at most once.  Calling one twice would run again a
part of the computation that has already run.

Generators appear in three places of the interface: @racket[make-amb]
takes the generator that supplies the alternatives of a choice;
@racket[amb-generate*] returns the generator of a search; and a
@tech{merger} makes one generator of two.  Programs that only use
@racket[amb], @racket[for/amb] and @racket[in-amb] never see one.


@section{Choices}

@defform[(amb expr ...)]{
John McCarthy's ambiguous operator: a choice among the @racket[expr]s.
Its value is the value of one of them, and the rest of the computation
runs once for each.  No @racket[expr] is evaluated before it is
chosen.

Each @racket[expr] is wrapped in a @racket[thunk], and the thunks are
collected into a mutable @tech/refer{vector}, on which
@racket[current-amb-shuffler] is called once.

@amb-examples[
(stream->list (in-amb (amb 1 2 3)))
]

@racket[(amb)] is the choice among no alternatives: it fails.  This is
commonly used to discard the computations that do not satisfy a
constraint.  It is the same as @racket[(amb-fail)], except that the
shuffler is called on the empty vector.

@amb-examples[
(stream->list (in-amb (amb)))
(stream->list (in-amb (amb 1 2 (amb) 3 4)))
(stream->list (in-amb (amb (amb 1 2 3) (amb 'a 'b 'c))))
]

@tech{Alternatives} producing multiple @racket[values] are supported:

@amb-examples[
(for/list ([(i j) (in-amb (amb (values 1 'x) (values 2 'y)))])
  (cons i j))
]
}

@deftogether[(@defform*[((for/amb maybe-length (for-clause ...) break-clause ... body ...+)
                         (for/amb type-ann-maybe maybe-length (for-clause ...) type-ann-maybe expr ...+))]
              @defform*[((for*/amb maybe-length (for-clause ...) break-clause ... body ...+)
                         (for*/amb type-ann-maybe maybe-length (for-clause ...) type-ann-maybe expr ...+))])]{
Variants of @racket[for] in which each iteration of the loop
@racket[body] is an @tech{alternative}.  The @racket[body] runs where
the choice was made, as part of the search: a choice in it is a choice
of the same search.

@amb-examples[
(for/list ([x (in-amb (for/amb ([x '(1 2)]) (amb x (- x))))]) x)
]

If the optional @racket[#:length] clause is specified, the syntax
resembles @racket[for/vector] and @racket[for*/vector]: the clauses
are run to their end where the choice is made, each loop @racket[body]
is wrapped in a @racket[thunk], and at most @racket[_length] of them
are collected into a mutable @tech/refer{vector}, on which
@racket[current-amb-shuffler] is called once.  The slots that no
iteration fills are not alternatives; with @racket[#:fill], they are,
each with the value of the fill expression.

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

If the optional @racket[#:length] clause is omitted, the iterations go
on as the search does: the clauses advance when the merger of the
choice turns to the next alternative.  This works with
@tech/refer{sequences} of any size, including infinite ones.  The
shuffler is not called.

@amb-examples[
(for/list ([x (in-amb (for/amb ([i (in-naturals)]) i))] [_ 5]) x)
(for/list ([x (in-amb (for/amb ([c "hello"]) c))]) x)
]

A @racket[for/amb] with no iterations fails, like @racket[(amb)].

The @racket[for-clause]s and @racket[break-clause]s are not part of
the search: see @secref["supply"].
}

@defproc[(sequence->amb [seq sequence?]) any]{
An element of @racket[seq]: a choice among all of them, which are
generated as the search goes on.  The sequence may be infinite.

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
Failure: the computation ends here, with no answer.

@amb-examples[
(stream->list (in-amb (amb 1 (amb-fail) 2)))
]
}

@defproc[(amb-pause) void?]{
A pause: the search reports a pause, and the computation goes on from
here when the search is asked for its next event.

A pause is where a @tech{merger} may pass the turn on to another
alternative.  With the default merger, @racket[amb-append], a pause
changes nothing but the events that an observer with a handler sees.
See @secref["fairness"].

@amb-examples[
(stream->list (in-amb (begin (amb-pause) 'x)))
(stream->list (in-amb (begin (amb-pause) 'x) #:on-pause (λ () 'pause)))
]
}

@defproc[(make-amb [next (-> any)]) any]{
The primitive that the other choices are made of: a choice among the
alternatives that the @tech{generator} @racket[next] supplies.  Each
answer of @racket[next] is the values of one alternative, which
@racket[make-amb] returns to its continuation.  The pauses of
@racket[next] are pauses of the choice, where they are.

@racket[(sequence->amb _seq)] is
@racket[(make-amb (λ () (sequence-generate* _seq)))].

The alternatives @racket[_a], @racket[_b] are merged from the right,
by the merger @racket[_μ] that is current when the choice is made, and
the last one is merged with the end of the supply:

@racketblock[
(_μ _K_a (_μ _K_b _end))
]

where @racket[_K_a] is the generator of the rest of the computation
with the alternative @racket[_a].  The inner merge is not made before
the outer one turns to it, so @racket[next] is not asked for an
alternative before its turn.  @racket[make-amb] does not leave out the
merge with the end, whatever the merger is; for a merger that has the
end as a right unit (all those of @racketmodname[amb/merge]), it makes
no difference.

@amb-examples[
(define (countdown n)
  (λ ()
    (if (zero? n)
        (values #f void)
        (values (list n) (countdown (sub1 n))))))
(stream->list (in-amb (make-amb (countdown 3))))
]

@racket[next] is code that supplies alternatives: it must not choose,
fail or pause in the search that it supplies (@secref["supply"]).  It
pauses by returning a number.
}

@defparam[current-amb-merger merge (-> (-> any) (-> any) (-> any))]{
A @tech/refer{parameter} that holds the @tech{merger} of the choices
that are made while it is the current one.  The default is
@racket[amb-append].

The merger is read where a choice is made, so different choices of one
search can have different mergers.

@amb-examples[
(for/list ([x (in-amb (list (parameterize ([current-amb-merger amb-alternate])
                              (amb 1 2))
                            (amb 'a 'b)))])
  x)
]
}

See @secref["mergers"] for the mergers of @racketmodname[amb/merge], and
for what a merger of one's own must do.

@defparam[current-amb-shuffler shuffle! (-> mutable-vector? void?)]{
A @tech/refer{parameter} that holds a procedure to put the
@tech{alternatives} of a choice in another order.  It is called once,
where the choice is made, on the vector of the alternatives of every
@racket[amb] and of every @racket[for/amb] with @racket[#:length]; the
vector may be empty.  It is not called for the alternatives of a
sequence.  The default is @racket[void].

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

The permutation is of all the alternatives of one choice.
@racket[(amb _a _b _c)] with a uniform shuffler is a uniform choice of
the first alternative; the nested @racket[(amb (amb _a _b) _c)] is
not.  The shuffler is code that supplies alternatives
(@secref["supply"]).
}


@section{Observing a Search}

@defform[(in-amb expr maybe-handler)
         #:grammar ([maybe-handler (code:line)
                                   (code:line #:on-pause handler-expr)])]{
Starts a @tech{search} of @racket[expr], and returns its
@tech{answers} as a lazy @tech/refer{stream}.  Nothing of the search
runs before the first element is asked for, and the stream remembers
its elements: reading them again does not run the search again.

When used in a @racket[for] clause, @racket[in-amb] is the single-pass
sequence of @racket[in-amb/do] directly, without the stream.

The @tech{pauses} of the search are skipped, unless
@racket[handler-expr] is given.  It must produce a procedure of no
arguments, which is called once for each pause (@racket[_n] times for
a batch of @racket[_n]); the values that it returns are an element of
the stream, in the place of the pause.  They do not go back into the
search.  The handler is called by whoever reads the stream, outside
the search.

@amb-examples[
(define s (in-amb (begin (amb-pause) (amb 1 2)) #:on-pause (λ () 'pause)))
(stream->list s)
(for/list ([x (in-amb (amb 1 (begin (amb-pause) 2) 3))]) x)
]
}

@defproc[(in-amb* [thk (-> any)] [handler (or/c #f (-> any)) #f]) stream?]{
The function form of @racket[in-amb]: the search of @racket[(thk)].
The pause handler is an optional argument.
}

@defform[(in-amb/do expr maybe-handler)]{
Like @racket[in-amb], but returns a single-pass
@tech/refer{sequence} instead of a stream: the answers are not
remembered.  Nothing of the search runs when the sequence is created;
iterating over it again goes on where the last iteration stopped, and
after its end it stays empty.
}

@defproc[(in-amb*/do [thk (-> any)] [handler (or/c #f (-> any)) #f]) sequence?]{
The function form of @racket[in-amb/do].
}

@subsection[#:tag "exceptions"]{After an Exception}

There are two cases, and they are different.

@itemlist[
@item{@bold{The pause handler raises.}  The pause is over.  The next
      read goes on with the search after that pause.}
@item{@bold{The computation of the search raises}, or control leaves
      it through a continuation jump.  The search cannot go on: what
      it had run cannot be run again.  The next read of the sequence
      raises @racket[exn:fail:contract] with the message
      @litchar{in-amb: the search cannot go on}; a stream reports that
      its delay is broken.}
]

@amb-examples[
(define-values (more? get)
  (sequence-generate (in-amb/do (amb 1 (error "boom") 3))))
(get)
(eval:error (get))
(eval:error (get))
]


@section[#:tag "local"]{A Search of One's Own}

@defproc[(amb-generate* [thk (-> any)]) (values (or/c list? #f exact-positive-integer?) (-> any))]{
Runs @racket[(thk)] as a @tech{search} of its own, up to its first
@tech{event}, and returns that event as a @tech{result}, with the
@tech{generator} of the rest: the shape of
@racket[sequence-generate*].  No effect of @racket[(thk)] reaches a
search around it, and nothing more of the search runs before the
generator is called.

@racket[amb-generate*] is eager: it runs the search up to its first
event when it is called.  @racket[(λ () (amb-generate* thk))] is the
generator of the whole search, and it is what @racket[in-amb*/do]
reads.

@amb-examples[
(define-values (v next) (amb-generate* (λ () (amb 1 (begin (amb-pause) 2)))))
v
(define-values (v2 next2) (next))
v2
(define-values (v3 next3) (next2))
v3
]

With @racket[make-amb], it gives the searches inside a search:
@racket[amb-local] below, and others that the library does not
provide, such as the first answer of a search.
}

@defform[(amb-local expr)]{
Equivalent to @racket[(amb-local* (λ () expr))].
}

@defproc[(amb-local* [thk (-> any)]) any]{
An answer of @racket[(thk)], searched for on its own: a @deftech{local
search}.  The choices of @racket[(thk)] are merged with each other
before the rest of the computation sees any of its answers, and these
answers are the alternatives of one choice.  It is

@racketblock[
(make-amb (λ () (amb-generate* thk)))
]

The pauses of the local search are pauses of the search around it.

With nothing after it, @racket[(amb-local expr)] has the events of
@racket[expr] itself under the mergers of @racketmodname[amb/merge].
With a continuation it need not: the answers are found before the
continuation runs, so their order can differ.  With
@racket[amb-append] it never does.

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

The first answer of a search can be written in two ways, which differ
in what happens to the pauses of that search.

@racketblock[
(define (first-answer thk)
  (for/first ([v (in-amb* thk)]) v))
]

Here the pauses stay in the search of @racket[(thk)]: for the search
around it, finding the answer is one step, and no merger gets a turn
before it is done.  If @racket[(thk)] pauses for ever, so does the
search around it, whatever its merger.

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
pause of the search around it, which stays fair; and no answer is a
failure.  This is the one that the committed choice of a miniKanren
needs.


@section[#:tag "mergers"]{Mergers}

@defmodule[amb/merge #:packages ("amb")]

A @deftech{merger} is a procedure that makes one @tech{generator} of
two.  It is the strategy of a choice.  The four mergers of this module
agree on answers and on the end of the first generator,

@verbatim[#:indent 2]{
(a : s) ∨ t  =  a : (s ∨ t)        (amb-alternate: a : (t ∨ s))
end     ∨ t  =  t
}

and differ in what a pause of the first generator means.

@defproc[(amb-append [s (-> any)] [t (-> any)]) (-> any)]{
All the events of @racket[s], then those of @racket[t]: depth-first
search.  A pause of @racket[s] is a pause of the merged generator, and
@racket[s] keeps the turn.  When @racket[s] has no end, @racket[t]
never runs.
}

@defproc[(amb-interleave [s (-> any)] [t (-> any)]) (-> any)]{
The turn passes on at each pause: when @racket[s] pauses, the merged
generator pauses, and goes on with @racket[t].  This is the
interleaving of miniKanren.
}

@defproc[(amb-alternate [s (-> any)] [t (-> any)]) (-> any)]{
The turn passes on at each event, an answer too.
}

@defproc[(amb-level [s (-> any)] [t (-> any)]) (-> any)]{
Breadth-first search by pauses.  When @racket[s] pauses, @racket[t]
runs up to its own pause, and then the merged generator pauses once:
both wait together.  Among several alternatives this gives rounds: in
a round, every alternative runs up to its next pause.
}

The merger of a choice is the value of @racket[current-amb-merger]
where the choice is made.

@subsection{What a Merger Must Do}

Any procedure of two generators that returns a generator can be the
value of @racket[current-amb-merger].  The library asks three things
of it.

@itemlist[
@item{It follows the @seclink["protocol"]{generator protocol}: its
      results are lists, @racket[#f] or positive integers; it calls
      each generator at most once; it does not call a generator after
      its end.}
@item{@bold{It does not depend on how pauses are written.}  Two
      generators with the same events, of which one returns
      @racket[3] where the other returns @racket[1] three times, must
      give merged generators with the same events.  The library itself
      rewrites batches of pauses, so a merger that looks at the size
      of a batch gives results that change from one version to the
      next.}
@item{@bold{It has no effects of a search.}  A merger must not call
      @racket[amb], @racket[amb-pause], @racket[amb-fail] or any other
      choice.  A merger is called by whoever asks the merged generator
      for an event, not by the search that it merges in; which search
      such an effect would belong to, if any, depends on when the
      merger happens to be called, and is not defined.  A merger
      pauses by returning a number.}
]

Here is @racket[amb-interleave], written out; it is the pattern of a
merger that handles a batch of @racket[_n] pauses.  It takes one pause
of the batch, passes the turn on, and leaves the other
@racket[(- _n 1)] in front of @racket[_s]:

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

The version that returns @racket[(values v (my-interleave t s))] for a
pause, passing the turn on once for a whole batch, is wrong: it gives
different answers for @racket[3] and for @racket[1], @racket[1],
@racket[1].

@subsection{Laws That a Merger May Have}

These are not required.  The mergers of this module have the first;
@racket[amb-append] and @racket[amb-level] have the second.  Events
are compared after every batch of pauses is written as single pauses.

@itemlist[
@item{@bold{The end is a unit.}  Merging with a generator that ends at
      once changes no event.  @racket[make-amb] merges the last
      alternative with the end of the supply; with this law that merge
      is invisible, so @racket[(amb _e)] has the events of
      @racket[_e], and a failing alternative can be added or removed.}
@item{@bold{Associativity.}  Nested choices have the events of one
      flat choice: @racket[(amb (amb _a _b) _c)] and
      @racket[(amb _a (amb _b _c))] are the same.  It does not hold
      for @racket[amb-interleave] and @racket[amb-alternate]: there,
      nesting decides how often each alternative gets the turn.}
]

Whatever the merger is, the rest of the computation is distributed
over the alternatives of a choice: @racket[(f (amb _a _b))] has the
events of @racket[(amb (f _a) (f _b))].  This needs no law.

A merger that gives every alternative the turn again and again is
@emph{fair}; fairness is a property of whole runs, not an equation.
@racket[amb-append] is not fair.

@subsection[#:tag "fairness"]{Fairness Needs Pauses}

A merger can pass the turn on only at an event.  A computation that
runs for ever without an answer and without a pause keeps the turn,
whatever the merger.  So a search with @racket[amb-interleave],
@racket[amb-alternate] or @racket[amb-level] finds every answer that
is at a finite depth, provided that

@itemlist[
@item{every choice has finitely many alternatives (but see below);}
@item{every step of the computation, and of the supply of
      alternatives, terminates; and}
@item{every infinite path of the computation has infinitely many
      pauses.}
]

The usual way to meet the last condition is to pause at the entry of
every recursive procedure, as miniKanren does at every relation.

A choice among infinitely many alternatives is outside these
conditions.  With @racket[amb-interleave] and @racket[amb-alternate]
it can still be searched when every alternative pauses before anything
else, as here; with @racket[amb-level] it cannot, because a round has
to start every alternative.

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


@section[#:tag "supply"]{The Code That Supplies Alternatives}

Some code runs for a choice without being part of the search: the
@tech{generator} of @racket[make-amb]; the sequence of
@racket[sequence->amb]; the @racket[_for-clause]s and
@racket[_break-clause]s of @racket[for/amb] and @racket[for*/amb]; the
procedure of @racket[current-amb-shuffler].  This code runs when the
merger of the choice turns to the next alternative, which may be long
after the choice was made, and in another dynamic context.  An effect
there has no meaning for the search, and is an error:

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

With @racket[#:length], the clauses are run to their end where the
choice is made, so the error is raised there, even if only the first
answer is asked for.  Without it, the error is raised when the search
gets to the iteration that has the effect.

@deftogether[(@defform[(with-amb-mark expr)]
              @defproc[(call-with-amb-mark [thk (-> any)]) any])]{
Evaluates @racket[expr], or calls @racket[thk], as code that supplies
alternatives: an effect of the search in it is an error.

@racket[make-amb] does this for its generator.  A form that collects
its alternatives before it calls @racket[make-amb], as
@racket[for/amb] with @racket[#:length] does, uses one of these for
the code that collects them; without it, that code would be part of
the search, and a choice in it would run the rest of the collection
once for each alternative.

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

What is protected is the body of a search and the continuations that
it captures.  Other code that runs when a generator is called, a
merger in particular, runs in the dynamic context of whoever calls the
generator; see @secref["mergers"].

The handler of @racket[#:on-pause] runs outside the search that it
observes.  An effect in it is an effect of the search around the
observer, if there is one.

Outside any search, an effect raises @racket[exn:fail:contract] with a
message that ends in @litchar{not in a search}.


@section[#:tag "base"]{The Core}

@defmodule[amb/base #:packages ("amb") #:no-declare]

@racketmodname[amb/base] exports the seven bindings that the rest is
made of, and nothing else: @racket[make-amb], @racket[amb-generate*],
@racket[amb-fail], @racket[amb-pause], @racket[current-amb-merger],
@racket[with-amb-mark] and @racket[call-with-amb-mark].
@racketmodname[amb] exports them too.  A library that builds its own
language on searches, such as a miniKanren, needs no more: a
disjunction is @racket[make-amb], a relation pauses with
@racket[amb-pause], and running a goal is @racket[amb-generate*].


@section[#:tag "v2"]{Changes from Version 2}

Version 3 is another design: a search is no longer a tree with a
frontier that a driver schedules, but a merge of generators, and the
strategy is a value that belongs to each choice.  Programs that only
use @racket[amb], @racket[for/amb], @racket[amb-fail] and
@racket[in-amb] with the default strategy behave as before.

@bold{The most visible change.}  In version 2,
@racket[current-amb-fair?] made a search complete over infinitely many
alternatives without any change to the program.  In version 3,
fairness comes from the pauses of the program
(@secref["fairness"]): such a program, moved to version 3 unchanged,
does not terminate.  To port it, choose @racket[amb-interleave] and
put an @racket[(amb-pause)] in every alternative of an infinite
choice, and at the entry of every recursive procedure that can recur
for ever without an answer.

The differences, one by one:

@itemlist[
@item{@bold{Version 2:} @racket[current-amb-depth-first?] set to @racket[#f] was breadth-first search by choices.  @bold{Version 3:} The parameter is removed.  @racket[amb-level] is breadth-first by pauses; in a program without pauses it is depth-first.}
@item{@bold{Version 2:} @racket[current-amb-fair?], set inside a computation, decided where the tree went back in the frontier of the whole search.  @bold{Version 3:} The parameter is removed.  @racket[current-amb-merger] belongs to the choice that is made under it, and merges the alternatives of that choice only.}
@item{@bold{Version 2:} The two parameters gave four strategies.  @bold{Version 3:} There are four mergers, which are other strategies.  On programs without pauses, depth-first is @racket[amb-append] and depth-first with fairness is @racket[amb-alternate]; the two breadth-first strategies have no counterpart.}
@item{@bold{Version 2:} @racket[(amb)] was answered by the handler of @racketidfont{#:empty-handler}, and abandoned the rest of the computation.  A pause was written @racket[(when (amb #t #f) (amb))].  @bold{Version 3:} @racket[(amb)] fails.  A pause is @racket[(amb-pause)], the computation goes on after it, and the handler is that of @racket[#:on-pause].}
@item{@bold{Version 2:} A choice with no alternatives, such as an empty @racket[for/amb], was answered by the handler too.  @bold{Version 3:} It fails.}
@item{@bold{Version 2:} The handler ran inside the search, where @racket[(amb)] was called.  @bold{Version 3:} The handler of @racket[#:on-pause] is called by the observer, outside the search.}
@item{@bold{Version 2:} Reaching @racket[(amb)] of the same search from its handler was a reentry error.  @bold{Version 3:} There is no such handler.  An effect in the handler of @racket[#:on-pause] belongs to the search around the observer.}
@item{@bold{Version 2:} The shuffler was not called for a choice with no alternatives.  @bold{Version 3:} It is called once for every vector of alternatives, the empty one too.}
@item{@bold{Version 2:} A fair search needed no pauses.  @bold{Version 3:} It needs them; see above.}
@item{@bold{Version 2:} @racket[make-amb] took two procedures, @racket[_more?] and @racket[_get].  @bold{Version 3:} It takes one @tech{generator}.  @racket[sequence->amb] is exported.}
@item{@bold{Version 2:} @racketidfont{amb/private/amb} exported @racketidfont{unsafe-amb*}.  @bold{Version 3:} The module is removed.}
@item{@bold{Version 2:} A choice or a failure in a clause of @racket[for/amb] did something that depended on the implementation.  @bold{Version 3:} It is an error (@secref["supply"]).}
@item{@bold{Version 2:} After an exception in the search, reading on ran the same part of the search again.  @bold{Version 3:} Reading on is an error (@secref["exceptions"]).}
]


@section{Typed Interface}

@racketmodname[typed/amb], in the package @tt{typed-amb}, gives types
to the bindings of @racketmodname[amb], and
@racketidfont{typed/amb/base} and @racketidfont{typed/amb/merge} to
those of @racketmodname[amb/base] and @racketmodname[amb/merge].

@itemlist[
@item{@racket[(Generatorof _a)] is the type of a @tech{generator}
      whose answers have the type @racket[_a], a type of lists:
      @racket[(→ (Values (∪ _a #f Positive-Integer) (Generatorof _a)))].
      @racketidfont{Generator} is @racket[(Generatorof (Listof Any))].}
@item{@racketidfont{Merger} is
      @racket[(→ Generator Generator Generator)], and
      @racket[current-amb-merger] is a @racket[(Parameter Merger)]: a
      merger is called before the search has answers of a known type.
      The four standard mergers are polymorphic in the answers, so
      they are @racketidfont{Merger}s, and keep the type of the
      generators that typed code applies them to.}
@item{@racket[for/amb] and @racket[for*/amb] take the type
      annotations of the second forms of their grammar above.}
]

On Racket 9.3, Typed Racket's @racket[for] type checker raises an
internal error for a clause with no identifiers over a zero-valued
sequence, including @racket[(in-amb/do (values))].  Zero-valued
computations themselves work: receive their values with
@racket[define-values] inside the search and return an ordinary element
for typed iteration.  This limitation does not affect untyped iteration.
