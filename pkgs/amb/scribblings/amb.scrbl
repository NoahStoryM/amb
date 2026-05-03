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
@racket[stream-flatten].  The examples above, rewritten in this style:

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
 (stream-flatten
  (for*/stream ([x (stream-flatten (stream 1 2))]
                [y (stream-flatten (stream 'a 'b))]
                [z (stream-flatten (stream x y))])
    z)))
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
    (time (stream->list (stream-flatten (stream:pythagorean n)))))
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

When called with no arguments, @racket[(amb)] signals failure, causing
the search to backtrack.  This is commonly used as a pruning mechanism
to discard branches that do not satisfy a constraint.

@amb-examples[
(stream->list (in-amb (amb 1 2 3)))
(stream->list (in-amb (amb)))
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

If the optional @racket[#:length] clause is omitted, the syntax
resembles @racket[for/stream] and @racket[for*/stream].  Each iteration
is lazily evaluated into a @tech/refer{stream}, which is then unpacked
via @racket[sequence-generate] and delegated to @racket[make-amb].
Since the body is evaluated on demand, this works with
@tech/refer{sequences} of any size, including infinite ones.

@amb-examples[
(for/list ([x (in-amb (for/amb ([i (in-naturals)]) i))] [_ 5]) x)
(for/list ([x (in-amb (for/amb ([c "hello"]) c))]) x)
]
}


@defproc[(make-amb [more? (-> boolean?)] [get (-> any)]) any]{
The foundational primitive for building ambiguous operators.
It captures the current continuation as an @tech{amb-tree}, then on
each resumption calls @racket[more?] to check for remaining
@tech{alternatives} and @racket[get] to produce the next
@racket[values].  When @racket[(more?)] returns @racket[#f],
@racket[make-amb] signals a dead end.

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

@defform[(in-amb expr)]{
Constructs a lazy @tech/refer{stream} of @tech{amb-tree} leaves from
the ambiguous expression @racket[expr].  All @racket[amb] operations
within @racket[expr] are delimited by a @tech/refer{prompt} installed
by @racket[in-amb].

The form @racket[(in-amb expr)] expands to
@racket[(in-amb* (λ () expr))].

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


@defproc[(in-amb* [thk (-> any)]) stream?]{
The procedural backend of @racket[in-amb].

@amb-examples[
(define (choices)
  (define x (amb 1 2 3))
  (define y (amb 'a 'b))
  (list x y))
(stream->list (in-amb* choices))
]
}

@defform[(in-amb/do expr)]{
Similar to @racket[in-amb], but returns a generic @tech/refer{sequence}
rather than specifically a lazy @tech/refer{stream}.
}

@defproc[(in-amb*/do [thk (-> any)]) sequence?]{
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
  @item{@emph{Leaf}: a result is produced.
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
