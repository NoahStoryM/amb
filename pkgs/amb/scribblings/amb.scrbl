#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/function
                     racket/sequence
                     racket/stream
                     racket/mutability
                     syntax/parse
                     (only-in srfi/43 vector-reverse!)
                     amb)
          "utils.rkt")

@title{amb: Ambiguous Operator}
@defmodule[amb       #:packages ("amb")]
@defmodule[typed/amb #:packages ("typed-amb") #:no-declare]
@author[@author+email["Noah Ma" "noahstorym@gmail.com"]]


The @racketmodname[amb] library provides John McCarthy's ambiguous
operator for nondeterministic programming.  All uses of @racket[amb]
operators must occur within the @tech/refer{dynamic extent} of
@racket[in-amb], which delimits an @tech{amb-tree}.  @racket[in-amb]
traverses the @tech{amb-tree} and returns its leaves as a lazy
@tech/refer{stream}.  The order of the leaves depends on the search
strategy (see @secref["Parameters"]).


@section{Evaluation Model}

An @deftech{amb-tree} is a @deftech{leaf-labeled tree}: internal nodes
are pure branching points with no values of their own; only leaves
carry the results of complete computations.  Structurally,
a tree is either a leaf value or a @tech/refer{stream} of subtrees.
Categorically, this is the @emph{free monad} over the
@tech/refer{stream} functor.

Each @racket[amb] expression creates an @tech{amb-tree} with one branch
per @deftech{alternative}.  A branch that calls @racket[(amb)] is a
@emph{dead end}: it is pruned and the search backtracks.

Sometimes it is useful to see where branches gave up: to count them, to
report them, or to mark a pause.  For that, a search can be given an
@deftech{empty handler}, a thunk passed as @racket[#:empty-handler] (see
@racket[in-amb]).  When a branch reaches @racket[(amb)], the handler is
called right there, and whatever it returns becomes one result of the
search.  The branch then stops, just as it would on failure:
@racket[(amb)] itself still does not return, so nothing after it in
that branch runs.

In the tree, an @racket[(amb)] node answered by the handler becomes a
leaf, written here as @tt{(amb) ⇒ (handler) ⇒} and the handler's
result.  With a handler that returns @racket['none]:

@amb-examples[
(stream->list (in-amb (amb (amb 1 2) (amb) (amb 'a 'b))
                      #:empty-handler (λ () 'none)))
]

@verbatim|{
(amb (amb 1 2) (amb) (amb 'a 'b))
├── (amb 1 2)
│   ├── 1
│   └── 2
├── (amb) ⇒ (handler) ⇒ 'none
└── (amb 'a 'b)
    ├── 'a
    └── 'b
}|

Because the branch stops at @racket[(amb)], the handler's result takes
the place of everything that branch would have computed.  Below, the
branch with @racket[x] = 1 produces @racket['none] instead of a list:

@amb-examples[
(stream->list
 (in-amb (let* ([x (amb 1 2)]
                [y (if (= x 1) (amb) (amb 'a 'b))])
           (list x y))
         #:empty-handler (λ () 'none)))
]

@verbatim|{
(amb 1 2)
├── (amb) ⇒ (handler) ⇒ 'none
└── (amb 'a 'b)
    ├── '(2 a)
    └── '(2 b)
}|

Two practical points follow.

@itemlist[
  @item{@emph{Choose a mark that cannot be mistaken for an answer.}
        The results do not say where each one came from, so if the
        handler returns a value that the computation could also
        produce, the two cannot be told apart.  A value of its own,
        such as an instance of a private structure type, is safest.}
  @item{@emph{Use @racket[(amb-fail)] for a failure that must stay a
        failure.}  The handler answers @racket[(amb)] but never
        @racket[(amb-fail)]; without a handler, the two are the same.
        For the same reason, @racket[(amb e (amb))] has the same
        results as @racket[e] only without a handler: with one, it has
        one more.}
  ]

@margin-note{Here @emph{a} is the type of the computation's results;
@emph{1} is a type with exactly one element (the @emph{terminal
object} of category theory), such as @racket[(Values)], whose only
element is @racket[(values)]; @emph{0} is a type with no elements (the
@emph{initial object}), that is, @racket[Nothing]; and @emph{b + a} is
a disjoint union, whose elements come from @emph{b} or from @emph{a},
tagged with their side.}
In terms of types, a handler that always returns the same mark has the
type @emph{1}, so the results of the search form a @tech/refer{stream}
of @emph{1 + a}: each is an answer, or the mark of a branch that ended
at @racket[(amb)].  A handler that returns @racket[(values)] also has
the type @emph{1}, and contributes a result of zero values.  The
default handler @racket[amb-fail] never returns, so its result type is
@emph{0}; since @emph{0 + a} has just the elements of @emph{a}, the
results are a @tech/refer{stream} of @emph{a}, as without a handler.
The first practical point above is that the consumer sees the values
but not the tags of the sum.

Internally, @racket[in-amb] maintains an @deftech{amb-frontier}—an
immutable deque of pending @tech{amb-trees}.  Each step pops a tree,
runs one branch, and, depending on the outcome, either yields a leaf
and suspends, enqueues a child tree and continues, discards the tree
and continues (dead end), or just continues.

The @tech{alternatives} in @racket[amb] are mutually exclusive—each is
lazily evaluated only when that branch is selected.

@amb-examples[
(stream->list (in-amb (amb (amb 1 2) (amb) (amb 'a 'b))))
]

Corresponding @tech{amb-tree}:

@verbatim|{
(amb (amb 1 2) (amb) (amb 'a 'b))
├── (amb 1 2)
│   ├── 1
│   └── 2
├── (amb)
└── (amb 'a 'b)
    ├── 'a
    └── 'b
}|

Nested @racket[amb] expressions are implicitly flattened: their
@tech{alternatives} are inserted into the parent's choice list.  Under
unfair depth-first order, @racket[(amb (amb 1 2) (amb) (amb 'a 'b))]
therefore produces the same sequence of leaves as
@racket[(amb 1 2 'a 'b)].

@amb-examples[
(stream->list (in-amb (amb 1 2 'a 'b)))
]

By contrast, when @racket[amb] expressions appear in order
(e.g., via @racket[let*]), each subsequent @racket[amb] is in the
@tech/refer{continuation} of the previous one.  The @tech{amb-tree}
becomes a Cartesian product of all choices.

@amb-examples[
(stream->list
 (in-amb
  (let* ([x (amb 1 2)]
         [y (amb 'a 'b)]
         [z (amb x y)])
    z)))
]

Corresponding @tech{amb-tree}:

@verbatim|{
(amb 1 2)
├── (amb 'a 'b)
│   ├── (amb 1 'a)
│   │   ├── 1
│   │   └── 'a
│   └── (amb 1 'b)
│       ├── 1
│       └── 'b
└── (amb 'a 'b)
    ├── (amb 2 'a)
    │   ├── 2
    │   └── 'a
    └── (amb 2 'b)
        ├── 2
        └── 'b
}|

Since this is the free monad over the @tech/refer{stream} functor, it
can be expressed purely with @tech/refer{streams}.  The correspondence
is direct: @racket[amb] is @racket[stream], @racket[in-amb] is
@racket[stream-flatten], and sequential @racket[amb] in @racket[let*]
corresponds to @racket[for*/stream] composed with
@racket[stream-flatten].  This is the interpretation without an
@tech{empty handler}, in which an empty choice contributes nothing; it
does not model the handler, whose leaf ends the whole branch.  The
examples above, rewritten in this style:

@racketblock[
(define (stream-flatten s*)
  (stream*
   (if (stream-empty? s*)
       s*
       (call-with-values
        (λ () (stream-first s*))
        (λ (v . v*)
          (define s (stream-rest s*))
          (if (and (null? v*) (stream? v))
              (if (stream-empty? v)
                  (stream-flatten s)
                  (stream-flatten (stream* (stream-first v) (stream-rest v) s)))
              (stream-cons (apply values v v*) (stream-flatten s))))))))
]

@amb-examples[
(code:comment "A single amb node with nested alternatives:")
(stream->list
 (stream-flatten (stream (stream 1 2) (stream) (stream 'a 'b))))
(code:comment "Sequential amb in let* form — the monadic bind —")
(code:comment "for*/stream with stream-flatten at each level:")
(stream->list
 (for*/stream ([x (stream-flatten (stream 1 2))]
               [y (stream-flatten (stream 'a 'b))]
               [z (stream-flatten (stream x y))])
   z))
]

The translation extends to realistic programs.  Whereas @racket[in-amb]
and nested @racket[amb] expressions both flatten implicitly, the
@tech/refer{stream} encoding requires explicit @racket[stream-flatten]
at each level.  Intermediate bindings use @racket[#:do], and
@racket[(amb)] (dead end) becomes @racket[#:when] or @racket[#:unless].

@amb-examples[
(define sequence->amb (compose make-amb sequence-generate))
(define (amb:pythagorean n)
  (define a (sequence->amb (in-range 1 n)))
  (define a² (* a a))
  (define b (sequence->amb (in-range a n)))
  (define b² (* b b))
  (define c (sequence->amb (in-range b n)))
  (define c² (* c c))
  (unless (= (+ a² b²) c²) (amb))
  (vector-immutable a b c))

(define (stream:pythagorean n)
  (for*/stream ([a (stream-flatten (in-range 1 n))]
                #:do [(define a² (* a a))]
                [b (stream-flatten (in-range a n))]
                #:do [(define b² (* b b))]
                [c (stream-flatten (in-range b n))]
                #:do [(define c² (* c c))]
                #:when (= (+ a² b²) c²))
    (vector-immutable a b c)))

(for/and ([n (in-inclusive-range 100 200 20)])
  (printf "pythagorean ~a:\n" n)
  (printf "  stream: ")
  (define stream:sol*
    (time (stream->list (stream:pythagorean n))))
  (printf "     amb: ")
  (define amb:sol*
    (time (stream->list (in-amb (amb:pythagorean n)))))
  (printf "  ~a solutions\n" (length stream:sol*))
  (equal? amb:sol* stream:sol*))
]

The difference is in the traversal mechanism.  @racket[stream-flatten]
relies on the implicit program stack as its frontier, yielding unfair
depth-first order (LIFO).  @racket[in-amb] replaces the stack with an
@tech{amb-frontier} to make the traversal order configurable
(see @secref["Parameters"]).

Despite the @tech/refer{continuation} capture and deque management,
the overhead is negligible in practice—on the Pythagorean triple
example above, the @racket[amb] version runs at roughly the same speed
as the @racket[stream] version.

The @racket[stream-flatten] wrappers in the @racket[stream] version
serve a specific purpose: they mirror @racket[amb]'s implicit
flattening of nested @tech{amb-trees}, so that each loop variable can
traverse a @tech{leaf-labeled tree} rather than just a flat
@tech/refer{stream}.  When the iteration source is already flat—as it
is for @racket[in-range] or @racket[in-stream] over a plain
@tech/refer{stream}—wrapping it in @racket[stream-flatten] adds
significant overhead.  In those cases, omit @racket[stream-flatten]
and iterate directly:

@amb-examples[
(define (pythagorean n)
  (for*/stream ([a (in-range 1 n)]
                #:do [(define a² (* a a))]
                [b (in-range a n)]
                #:do [(define b² (* b b))]
                [c (in-range b n)]
                #:do [(define c² (* c c))]
                #:when (= (+ a² b²) c²))
    (vector-immutable a b c)))
(for ([n (in-inclusive-range 100 200 20)])
  (printf "pythagorean ~a:\n  " n)
  (define sol* (time (stream->list (pythagorean n))))
  (printf "  ~a solutions\n" (length sol*)))
]


@section{Ambiguous Operator}

@defform[(amb expr ...)]{
John McCarthy's ambiguous operator.  It creates an @tech{amb-tree} to
nondeterministically choose among the given @racket[expr]s.

Each @racket[expr] is implicitly wrapped in a @racket[thunk] and
collected into a mutable @tech/refer{vector}.

When called with no arguments, @racket[(amb)] is a choice among no
@tech{alternatives}: the computation ends there.  What it ends with is
decided by the innermost search.  By default it ends with nothing, so
@racket[(amb)] signals failure, causing the search to backtrack; this
is commonly used as a pruning mechanism to discard branches that do
not satisfy a constraint.  A search started with a handler
(@racket[#:empty-handler] for @racket[in-amb] and @racket[in-amb/do]) instead
calls the handler at the point of @racket[(amb)] and produces its
results as one leaf.

@amb-examples[
(stream->list (in-amb (amb 1 2 3)))
(stream->list (in-amb (amb)))
(stream->list (in-amb (amb) #:empty-handler (λ () 'none)))
]

A dead-end @racket[(amb)] nested among @tech{alternatives} prunes only
that branch—the others proceed normally:

@amb-examples[
(stream->list (in-amb (amb 1 2 (amb) 3 4)))
]

A productive nested @racket[amb] creates a child @tech{amb-tree}.
When the inner tree is exhausted, the search backtracks and continues
with the next sibling in the parent:

@amb-examples[
(stream->list (in-amb (amb (amb 1 2 3) (amb 'a 'b 'c))))
]

@tech{Alternatives} producing multiple @racket[values] are supported:

@amb-examples[
(for/list ([(i j) (in-amb (amb (values 1 'x) (values 2 'y)))])
  (cons i j))
]
}

@defproc[(amb-fail) none/c]{
Signals failure unconditionally: the branch is a @emph{dead end} even
in a search that has a handler for @racket[(amb)].  Without a handler,
@racket[(amb-fail)] and @racket[(amb)] behave the same.

@amb-examples[
(stream->list (in-amb (amb 1 (amb) (amb-fail) 2) #:empty-handler (λ () 'none)))
]
}


@deftogether[(@defform*[((for/amb maybe-length (for-clause ...) break-clause ... body ...+)
                         (for/amb type-ann-maybe maybe-length (for-clause ...) type-ann-maybe expr ...+))]
              @defform*[((for*/amb maybe-length (for-clause ...) break-clause ... body ...+)
                         (for*/amb type-ann-maybe maybe-length (for-clause ...) type-ann-maybe expr ...+))])]{
Variants of @racket[for] that treat each iteration of the loop
@racket[body] as an @tech{alternative}.

If the optional @racket[#:length] clause is specified, the syntax
resembles @racket[for/vector] and @racket[for*/vector].  Each loop
@racket[body] is implicitly wrapped in a @racket[thunk] and collected
into a mutable @tech/refer{vector}, mirroring how @racket[amb] itself
works.

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

If the optional @racket[#:length] clause is omitted, the number of
iterations is unknown, so each loop @racket[body] is deferred to a
@racket[thunk] and produced one at a time by driving the @racket[for]
loop as a coroutine: a captured continuation resumes the loop to yield
the next @tech{alternative} on demand.  Because the @racket[body] is
evaluated lazily, this works with @tech/refer{sequences} of any size,
including infinite ones.

A @racket[for/amb] that provides no @tech{alternative} at all is
@racket[(amb)]: without @racket[#:length], a loop with no iterations;
with @racket[#:length], a length of 0, or no iterations and no
@racket[#:fill].  Slots filled by default are not alternatives, but
with @racket[#:fill] every slot is one.  The clauses are evaluated
once either way.

@amb-examples[
(stream->list (in-amb (for/amb ([i '()]) i) #:empty-handler (λ () 'none)))
(stream->list (in-amb (for/amb #:length 2 ([i '()]) i) #:empty-handler (λ () 'none)))
(stream->list (in-amb (for/amb #:length 2 #:fill 0 ([i '()]) i) #:empty-handler (λ () 'none)))
]

@amb-examples[
(for/list ([x (in-amb (for/amb ([i (in-naturals)]) i))] [_ 5]) x)
(for/list ([x (in-amb (for/amb ([c "hello"]) c))]) x)
(for/list ([x (in-amb (for/amb ([x '(1 2)]) (amb x (- x))))]) x)
]
}


@defproc[(make-amb [more? (-> boolean?)] [get (-> any)]) any]{
The foundational primitive for building ambiguous operators.
It captures the current continuation as an @tech{amb-tree}, then on
each resumption calls @racket[more?] to check for remaining
@tech{alternatives} and @racket[get] to produce the next
@racket[values].  When @racket[(more?)] returns @racket[#f],
@racket[make-amb] signals a dead end; if it does so before any
@tech{alternative} has been produced, @racket[make-amb] is
@racket[(amb)].

@amb-examples[
(define sequence->amb (compose make-amb sequence-generate))
(define (roll-doubles)
  (define a (sequence->amb (in-range 1 7)))
  (define b (sequence->amb (in-range 1 7)))
  (unless (= a b) (amb))
  (list a b))
(stream->list (in-amb (roll-doubles)))
]
}


@section{Stream Constructor}

@defform[(in-amb expr maybe-empty)
         #:grammar ([maybe-empty (code:line)
                                 (code:line #:empty-handler handler-expr)])
         #:contracts ([handler-expr (-> any)])]{
Constructs a lazy @tech/refer{stream} of @tech{amb-tree} leaves from
the ambiguous expression @racket[expr].  All @racket[amb] operations
within @racket[expr] are delimited by a @tech/refer{prompt} installed
by @racket[in-amb].

The form @racket[(in-amb expr)] expands to
@racket[(in-amb* (λ () expr))], and
@racket[(in-amb expr #:empty-handler handler-expr)] to
@racket[(in-amb* (λ () expr) handler-expr)].

The @tech{empty handler} answers @racket[(amb)].  When the computation
reaches @racket[(amb)], the handler is called there, in the dynamic
context of @racket[(amb)], and the values it returns are produced as
one leaf; the rest of that branch is abandoned, as with failure.  The
default handler is @racket[amb-fail], which makes @racket[(amb)] a
failure.  The handler may also call @racket[amb-fail] itself, make
choices of its own, or raise an exception; if it reaches @racket[(amb)]
of the same search, @racket[exn:fail:contract] is raised.  It should return as many
values as @racket[expr] does: Typed Racket checks this, and otherwise
the mismatch is reported only where the leaf is received.

Since the search is lazy, the dynamic context of @racket[(amb)] is the
one in which the search is resumed, that is, the consumer's, as for any
other code in @racket[expr]:

@amb-examples[
(define p (make-parameter 'first))
(define-values (more? get)
  (sequence-generate
   (in-amb/do (begin (when (amb #t #f) (amb)) (when (amb #t #f) (amb)) 'x)
              #:empty-handler p)))
(list (get) (parameterize ([p 'second]) (get)) (get))
]

@amb-examples[
(stream->list (in-amb (let ([x (amb 1 2 3)]) (if (even? x) (amb) x))
                      #:empty-handler (λ () 'even)))
]

The handler belongs to the search that was given it, not to the
dynamic extent of @racket[in-amb]: a search started inside the
computation, without a handler of its own, still fails at its own
@racket[(amb)].  Together with a choice point, a handler makes a
@emph{pause} observable: @racket[(when (amb #t #f) (amb))] produces the
handler's leaf and then continues the computation after it.

@amb-examples[
(define (pause) (when (amb #t #f) (amb)))
(stream->list (in-amb (begin (pause) 'done) #:empty-handler (λ () 'paused)))
(stream->list (in-amb (begin (pause) 'done)))
]

@amb-examples[
(stream->list (in-amb (amb 1 (amb 2 3) (amb 4 5 6) 7 8)))
]

@racket[in-amb] acts as a one-step lookahead @tech/refer{stream}
constructor.  To determine whether the @tech/refer{stream} is
non-empty, it eagerly evaluates the next leaf.

@amb-examples[
(for/list ([i (in-amb (begin0 (amb 1 (amb 2 3) 4) (displayln 'hi)))]
           [_ 2])
  i)
(for/list ([_ 2]
           [i (in-amb (begin0 (amb 1 (amb 2 3) 4) (displayln 'hi)))])
  i)
]
}


@defproc[(in-amb* [thk (-> any)] [handler (-> any) amb-fail]) stream?]{
The procedural backend of @racket[in-amb]; @racket[handler] answers
@racket[(amb)], as with @racket[#:empty-handler].

@amb-examples[
(define (choices)
  (define x (amb 1 2 3))
  (define y (amb 'a 'b))
  (list x y))
(stream->list (in-amb* choices))
]
}

@defform[(in-amb/do expr maybe-empty)
         #:grammar ([maybe-empty (code:line)
                                 (code:line #:empty-handler handler-expr)])
         #:contracts ([handler-expr (-> any)])]{
Similar to @racket[in-amb], but returns a generic @tech/refer{sequence}
rather than specifically a lazy @tech/refer{stream}.  The sequence is
single-pass: results are not retained, so iterating it consumes them,
and once every result has been consumed the sequence is empty.  This
makes it cheaper than @racket[in-amb] when the results are needed only
once.
}

@defproc[(in-amb*/do [thk (-> any)] [handler (-> any) amb-fail]) sequence?]{
The procedural backend of @racket[in-amb/do].
}


@section{Parameters}

The behavior of the @racket[amb] search engine is governed by two
orthogonal @tech/refer{parameters}: @racket[current-amb-depth-first?]
controls traversal order and @racket[current-amb-fair?] controls
fairness.  Together, they make the @tech{amb-frontier} pluggable in
ways impossible to achieve with the implicit stack alone.

As described in @secref["Evaluation_Model"], the search driver
maintains an @tech{amb-frontier}.  Each step pops an @tech{amb-tree}
from the front and runs it.  The result determines what happens next:

@itemlist[
  @item{@emph{Dead end}: the tree is exhausted and is discarded
        (not returned to the @tech{amb-frontier}).
        The search continues.}
  @item{@emph{Nothing}: the tree produced nothing on this resumption.
        The parent tree is put back for its remaining
        @tech{alternatives}.
        @racket[current-amb-fair?] controls whether the parent
        tree goes to the front or the back.
        The search continues.}
  @item{@emph{Leaf}: a result is produced, by the computation or by
        the @tech{empty handler} at @racket[(amb)].
        The parent tree is put back (as in the nothing case).
        The search suspends.}
  @item{@emph{Branch}: a new @tech{amb-tree} was created.
        The parent tree is put back (as in the nothing case), and the
        new child tree is also inserted.
        @racket[current-amb-depth-first?] controls whether the child
        tree goes to the front or the back.
        The search continues.}
  ]

Both @tech/refer{parameters} are captured per-tree at the point where
@racket[amb] or @racket[for/amb] is evaluated.  This means different
parts of a computation can use different strategies via
@racket[parameterize].  As a result, different subtrees may coexist in
the same search with different traversal strategies.


@defparam[current-amb-depth-first? depth-first? boolean?]{
Controls where a newly created child tree is inserted in the
@tech{amb-frontier}.

When @racket[#t] (the default), child trees are pushed to the front of
the @tech{amb-frontier}, yielding depth-first search (DFS).
When @racket[#f], child trees are pushed to the back, yielding
breadth-first search (BFS).

@amb-examples[
(stream->list (in-amb (amb 1 (amb 2 3) (amb 4 5 6) 7 8)))
(parameterize ([current-amb-depth-first? #f])
  (stream->list (in-amb (amb 1 (amb 2 3) (amb 4 5 6) 7 8))))
]
}


@defparam[current-amb-fair? fair? boolean?]{
Controls where the parent tree is placed back in the @tech{amb-frontier}
after being popped and producing a result.

When @racket[#f] (the default), the parent tree is placed back at the
front.  When @racket[#t], the parent tree is placed at the back,
giving other pending trees a chance to run first.  This prevents any
single branch from starving its siblings.

@amb-examples[
(stream->list
 (in-amb
  (let* ([a (amb 1 2 3)]
         [b (amb 4 5 6)])
    (list a b))))
(stream->list
 (in-amb
  (let ([a (amb 1 2 3)])
    (parameterize ([current-amb-fair? #t])
      (define b (amb 4 5 6))
      (list a b)))))
]
}


@defparam[current-amb-shuffler shuffle! (-> mutable-vector? void?)]{
Controls how to shuffle @racket[thunk]s before creating an
@tech{amb-tree} via @racket[amb].  The procedure receives the mutable
@tech/refer{vector} of @racket[thunk]s and may reorder them in place.

The default value is @racket[void] (no shuffling).

@amb-examples[
(require srfi/43)
(parameterize ([current-amb-shuffler vector-reverse!])
  (stream->list (in-amb (amb 1 2 3 4 5))))
]
}
