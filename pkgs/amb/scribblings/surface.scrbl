#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/function
                     racket/sequence
                     racket/stream
                     racket/mutability
                     (only-in srfi/43 vector-reverse!)
                     amb
                     amb/base
                     amb/merge)
          "utils.rkt")

@title[#:tag "surface"]{Choices, Observers, and Local Searches}

@declare-exporting[amb]

The @racketmodname[amb] module provides the surface choice forms,
search observers, and local search combinators, and re-exports the core
primitives of @racketmodname[amb/base] (@secref["base"]).


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

An @tech{alternative} may return multiple @racket[values].

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
                         (for/amb type-ann-maybe maybe-length (for-clause ...) type-ann-maybe break-clause ... body ...+))]
              @defform*[#:literals (:)
                        ((for*/amb maybe-length (for-clause ...) break-clause ... body ...+)
                         (for*/amb type-ann-maybe maybe-length (for-clause ...) type-ann-maybe break-clause ... body ...+))
                        #:grammar ([maybe-length (code:line)
                                                 (code:line #:length length-expr)
                                                 (code:line #:length length-expr #:fill fill-expr)]
                                   [break-clause (code:line #:break guard-expr)
                                                 (code:line #:final guard-expr)]
                                   [type-ann-maybe (code:line)
                                                   (code:line : type)])])]{
Variants of @racket[for] and @racket[for*] in which each iteration of
the loop @racket[body] is an @tech{alternative}.  The @racket[body]
runs where the choice was made, as part of the search: a choice in it
is a choice of the same search.  The second form of each pair accepts
optional Typed Racket type annotations (@secref["typed"]).

@amb-examples[
(for/list ([x (in-amb (for/amb ([x '(1 2)]) (amb x (- x))))]) x)
]

@bold{With @racket[#:length]}, the forms work like @racket[amb]
combined with @racket[for/vector] and @racket[for*/vector].  Where the
choice is made, @racket[length-expr] is evaluated and the clauses run
until the loop finishes or reaches that length.  Each loop
@racket[body] is wrapped in a @racket[thunk], and at most
@racket[_length] thunks are collected into a mutable
@tech/refer{vector}, on which @racket[current-amb-shuffler] is called
once.

The slots that no iteration fills are not alternatives, unless
@racket[#:fill] is given: then @racket[fill-expr] is wrapped in a
@racket[thunk] and each empty slot becomes an alternative that
evaluates @racket[fill-expr] when chosen (unlike @racket[for/vector],
@racket[fill-expr] is evaluated lazily per chosen slot, not once up
front).  A length of zero skips the clauses and calls the shuffler on
an empty vector.

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

@bold{Without @racket[#:length]}, the loop does not collect its
alternatives up front; instead, it advances lazily as the search does,
taking a step through the clauses when the merger of the choice turns
to the next alternative.  This works with @tech/refer{sequences} of
any size, infinite ones included.  The shuffler is @bold{not} called.

@amb-examples[
(for/list ([x (in-amb (for/amb ([i (in-naturals)]) i))] [_ 5]) x)
(for/list ([x (in-amb (for/amb ([c "hello"]) c))]) x)
]

With no iterations, @racket[for/amb] fails unless a positive length
and @racket[#:fill] supply alternatives.

The @racket[for-clause]s and @racket[break-clause]s are not part of
the search; see @secref["supply"].
}

@defproc[(sequence->amb [seq sequence?]) any]{
A choice among the elements of @racket[seq], which are produced as
the search goes on.  The @tech/refer{sequence} may be infinite.

@amb-examples[
(define (roll-doubles)
  (define a (sequence->amb (in-range 1 7)))
  (define b (sequence->amb (in-range 1 7)))
  (unless (= a b) (amb))
  (list a b))
(stream->list (in-amb (roll-doubles)))
]
}

@defparam[current-amb-shuffler shuffle! (-> mutable-vector? void?)]{
A @tech/refer{parameter} that holds a procedure to reorder the
@tech{alternatives} of a choice.  It is called once, where the choice
is made, on the vector of alternatives of every @racket[amb] and of
every @racket[for/amb] with @racket[#:length]; the vector may be
empty.  It is not called for the alternatives of a @tech/refer{sequence}.
The default is @racket[void].

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
runs before the first element is requested, and the @tech/refer{stream}
keeps its elements: reading them again does not run the search again.

In a @racket[for] clause, @racket[in-amb] is used directly as the
single-pass @tech/refer{sequence} of @racket[in-amb/do], without
building the @tech/refer{stream}.

The @tech{pauses} of the search are skipped, unless
@racket[handler-expr] is given.  It must produce a @racket[thunk] (or
@racket[#f] to skip pauses), which is called once for each pause
(@racket[_n] times for a batch of @racket[_n]).  The values that it
returns become one element of the @tech/refer{stream}, in place of the
pause; they do not go back into the search.  The handler is called by
whoever reads the @tech/refer{stream}, outside the search.

@amb-examples[
(define (p) 'pause)
(for/list ([x (in-amb (begin (amb-pause) (amb 1 2)) #:on-pause p)]) x)
(for/list ([x (in-amb (amb 1 (begin (amb-pause) 2) 3))]) x)
]
}

@defproc[(in-amb* [thk (-> any)] [handler (or/c #f (-> any)) #f]) stream?]{
The procedure form of @racket[in-amb]: the search of
@racket[(thk)].  The pause handler is an optional argument.  Like
@racket[in-amb], in a @racket[for] clause it expands directly to the
single-pass @tech/refer{sequence} of @racket[in-amb*/do], without
building a @tech/refer{stream}.
}

@defform[(in-amb/do expr maybe-handler)
         #:grammar ([maybe-handler (code:line)
                                   (code:line #:on-pause handler-expr)])]{
Like @racket[in-amb], but returns a single-pass
@tech/refer{sequence} instead of a @tech/refer{stream}, so the answers
are not kept.  Creating the @tech/refer{sequence} runs nothing of the
search.  Iterating over it again continues where the previous
iteration stopped, and once it has ended it stays empty.
}

@defproc[(in-amb*/do [thk (-> any)] [handler (or/c #f (-> any)) #f]) sequence?]{
The procedure form of @racket[in-amb/do].
}

See @secref["exceptions"] for how @racket[in-amb] and @racket[in-amb/do]
behave after an exception or a reentrant read.


@section[#:tag "local"]{Local Searches and First Answers}

@defform[(amb-local expr)]{
Equivalent to @racket[(amb-local* (λ () expr))].
}

@defproc[(amb-local* [thk (-> any)]) any]{
An answer of @racket[(thk)], found by a search of its own: a
@deftech{local search}.  The inner mergers decide the order of its
events.  The local search runs on demand, supplying each answer as
an alternative of one choice in the enclosing search.  It is the
composition of @racket[make-amb] and @racket[amb-generate*]:

@racketblock[
(make-amb (λ () (amb-generate* thk)))
]

The pauses of the local search are pauses of the search around it.

Within the search-event model (fixing the same inner computation and
mergers, and assuming the computation does not observe host dynamic
environment differences such as parameters across the search
boundary; see @secref["host-code"]):

@itemlist[
@item{At the end of a computation, @racket[(amb-local expr)] has the
      same @tech{logical events} as @racket[expr] when the enclosing
      choice uses one of the mergers of @racketmodname[amb/merge].}
@item{When the computation goes on after it, the order can change: the
      enclosing merger schedules the rest of the computation from the
      local search's answers.  If @racket[amb-append] is used
      throughout, both forms have the same logical events for any
      continuation.}
]

@amb-examples[
(require amb/merge)
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
