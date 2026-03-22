#lang scribble/manual

@(require (for-label racket/base
                     racket/function
                     racket/contract/base
                     racket/sequence
                     racket/stream
                     racket/mutability
                     syntax/parse
                     data/queue
                     goto
                     amb)
          "utils.rkt")

@title{amb: Ambiguous Operator}
@defmodule[amb       #:packages ("amb")]
@defmodule[typed/amb #:packages ("typed-amb") #:no-declare]
@author[@author+email["Noah Ma" "noahstorym@gmail.com"]]

@section{Ambiguous Operator}

@defform[(amb expr ...)]{
John McCarthy's ambiguous operator.

The form @racket[(amb expr ...)] expands to @racket[(amb* (λ () expr) ...)].

@amb-examples[
(amb 1 (values 2 3) 4)
(amb)
(amb)
(eval:error (amb))
]

Wrapping @racket[amb] expressions with a new @tech{amb sequence} is
recommended. This ensures that each instance of non-deterministic
computation starts with a fresh @tech/refer{sequence}, avoiding
unintended interactions between different @racket[amb] expressions.

@amb-examples[
(define make-amb-tasks (current-amb-maker))
(amb 1 (values 2 3) 4)
(eval:error
 (parameterize ([current-amb-tasks (make-amb-tasks)])
   (let ([x (amb 1 2)])
     (displayln (list x))
     (let ([y (amb 3 4)])
       (displayln (list x y))
       (let ([z (amb 5 6)])
         (displayln (list x y z))
         (amb))))))
(amb)
(eval:error
 (parameterize ([current-amb-tasks (make-amb-tasks)]
                [current-amb-pusher enqueue!])
   (let ([x (amb 1 2)])
     (displayln (list x))
     (let ([y (amb 3 4)])
       (displayln (list x y))
       (let ([z (amb 5 6)])
         (displayln (list x y z))
         (amb))))))
(amb)
(eval:error (amb))
]

For most use cases, we recommend using @racket[in-amb] to construct
a @racket[stream] from ambiguous computations. A good practice is to
wrap @racket[amb] expressions in a procedure, then use @racket[in-amb]
to create a lazy @tech/refer{stream} that produces as many results as
needed.

@amb-examples[
(define (f n)
  (define m (amb 0 1 2 3 4))
  (unless (> m n) (amb))
  m)
(for/list ([m (in-amb (f 2))]) m)
]
}

@defproc[(amb* [alt (-> any)] ...) any]{
The backend of @racket[amb].

If an @racket[alt] is the @racket[amb*] procedure itself, it is
skipped, and evaluation proceeds to the next @racket[alt].

@amb-examples[
(define (zero) 0)
(define (one) 1)
(current-amb-shuffler displayln)
(amb* amb* zero amb* one amb*)
(amb)
(eval:error (amb))
(define (amb+) (amb))
(amb* amb+ zero amb+ one amb+)
(amb)
(eval:error (amb))
]
}

@deftogether[(@defform*[((for/amb maybe-length (for-clause ...) break-clause ... body ...+)
                         (for/amb type-ann-maybe maybe-length (for-clause ...) type-ann-maybe expr ...+))]
              @defform*[((for*/amb maybe-length (for-clause ...) break-clause ... body ...+)
                         (for*/amb type-ann-maybe maybe-length (for-clause ...) type-ann-maybe expr ...+))])]{
Variants of @racket[for] that treat each iteration of the loop body as
an ambiguous choice.

When the @racket[#:length] clause is specified, the syntax resembles
@racket[for/vector] and @racket[for*/vector]. The loop body for each
iteration is wrapped in a @racket[thunk] to create a fixed number of
@deftech{alternative}s.

@amb-examples[
(for/amb #:length 4 ([i #(1 2)] [j #(x y)]) (values i j))
(amb)
(eval:error (amb))
(for/amb #:length 4 #:fill 0 ([i #(1 2)] [j #(x y)]) (values i j))
(amb)
(amb)
(amb)
(eval:error (amb))
]

If the @racket[#:length] clause is omitted, a @deftech{choice point}
is created for each iteration. After the body of one iteration is
evaluated, backtracking via @racket[(amb)] proceeds to the next
iteration. This allows for iterating over @tech/refer{sequences} of
any size, including infinite ones.

@amb-examples[
(for/amb ([i (in-naturals)]) i)
(amb)
(amb)
(amb)
]
}

@section{Prompt Tag}

@defthing[amb-prompt-tag continuation-prompt-tag?]{
A continuation prompt tag used by @racket[in-amb] operations to
delimit backtracking.
}

@section{Stream Constructor}

@defform[(in-amb expr)]{
Constructs a @tech/refer{stream} from the results of evaluating the
ambiguous expression @racket[expr], allowing results to be produced
lazily.

The form @racket[(in-amb expr)] expands to @racket[(in-amb* (λ () expr))].

@amb-examples[
(for/list ([i (in-amb (amb 1 (amb 2 3) (amb 4 5 6) 7 8))]) i)
]

@racket[in-amb] acts as a one-step lookahead @tech/refer{sequence}
generator. To determine if the @tech/refer{sequence} continues,
it must compute the next valid path.

@amb-examples[
(for/list ([i (in-amb (begin0 (amb 1 (amb 2 3) 4) (displayln 'data)))] [_ 2]) i)
(for/list ([_ 2] [i (in-amb (begin0 (amb 1 (amb 2 3) 4) (displayln 'data)))]) i)
]

The ambiguous computation is executed in the dynamic context where
@racket[in-amb] is called. In particular, dynamic bindings such as
those introduced by @racket[parameterize] are preserved from the
@tech/refer{stream}'s creation site rather than from the site where
the @tech/refer{stream} is consumed.

@amb-examples[
(define p (make-parameter 0))
(define s (parameterize ([p 1]) (in-amb (amb 0 (p) 2))))
(for/list ([i (parameterize ([p 2]) s)]) i)
]

Internally, @racket[in-amb] installs the @tech/refer{parameters}
required to run an ambiguous computation, including
@racket[current-amb-prompt-tag], @racket[current-amb-tasks], and
@racket[current-amb-empty-handler]. Other @tech/refer{parameters}
retain the values they had when @racket[in-amb] was invoked.

@amb-examples[
(amb 1 2 3)
(for/list ([(i j) (in-amb (amb (values 1 'x) (values 2 'y)))]) (cons i j))
(amb)
(define (rotate-queue! q)
  (when (non-empty-queue? q)
    (enqueue! q (dequeue! q))))
(parameterize ([current-amb-rotator rotate-queue!])
  (for/list ([i (in-amb (amb (amb 1 2 3) (amb 'a 'b 'c)))]) i))
(amb)
(eval:error (amb))
]
}

@defproc[(in-amb* [alt (-> any)]) stream?]{
The backend of @racket[in-amb].

If @racket[alt] is @racket[amb*], @racket[empty-stream] is returned.

@amb-examples[
(define (make)
  (parameterize ([current-amb-pusher enqueue!])
    (let g () (amb 0 1 (cons (g) (g))))))
(for/list ([x (in-amb* make)] [_ 10]) x)
(eq? (in-amb* amb*) empty-stream)
]
}

@defform[(in-amb/do expr)]{
Similar to @racket[in-amb], except that a @tech/refer{sequence} is
returned instead of a @tech/refer{stream}.
}

@defproc[(in-amb*/do [alt (-> any)]) sequence?]{
The backend of @racket[in-amb/do].

If @racket[alt] is @racket[amb*], @racket[empty-sequence] is returned.

@amb-examples[
(eq? (in-amb*/do amb*) empty-sequence)
]
}

@section{Exception Type}

@defstruct[(exn:fail:contract:amb exn:fail:contract) ()
           #:inspector #f]{
Raised when evaluating @racket[(amb)] with an empty @tech{amb sequence}.

@amb-examples[
(eval:error (amb))
(eval:error (amb*))
(eval:error (for/amb ([i '()]) i))
(eval:error (for*/amb ([i '()]) i))
]
}

@defproc[(raise-amb-error) none/c]{
Creates an @racket[exn:fail:contract:amb] value and @racket[raise]s it
as an @tech/guide{exception}.

@amb-examples[
(eval:error (raise-amb-error))
]
}

@section{Parameter}

The behavior of the @racket[amb] search engine can be customized
through a set of @tech/refer{parameters}. These @tech/refer{parameters}
control how @tech{amb tasks} are stored, scheduled, and executed, as
well as the continuation prompt used to delimit ambiguous computations.

@defparam[current-amb-prompt-tag prompt-tag continuation-prompt-tag?]{
A @tech/refer{parameter} that specifies the continuation prompt tag
used to delimit ambiguous computations.
The default value is @racket[(default-continuation-prompt-tag)].
}

@defparam[current-amb-empty-handler empty-handler (-> none/c)]{
A @tech/refer{parameter} that specifies the procedure to be called
when the @tech{amb sequence} is empty and @racket[(amb)] is evaluated.
The default value is @racket[raise-amb-error].
}

@defparam[current-amb-shuffler shuffle! (-> mutable-vector? void?)]{
A @tech/refer{parameter} that specifies how to shuffle
@tech{alternatives} before scheduling new @tech{amb tasks} into the
current @tech{amb sequence}.
The default value is @racket[void].
}

@defparam[current-amb-rotator rotate! (-> sequence? void?)]{
A @tech/refer{parameter} that specifies how to rotate @tech{amb tasks}
before running an @tech{alternative}.
The default value is @racket[void].
}

@defparam[current-amb-maker make (-> sequence?)]{
A @tech/refer{parameter} that specifies the method for creating a new
@deftech{amb sequence}. This allows users to define the data structure
used to store @tech{amb tasks}.
The default value is @racket[make-queue].
}

@defparam[current-amb-tasks tasks sequence?]{
A @tech/refer{parameter} that holds the @tech/refer{sequence} of
@tech{amb tasks} to be evaluated. Each @deftech{amb task} is a
@tech/refer{continuation} created by @racket[current-continuation].
The default value is @racket[(make-queue)].
}

@defparam[current-amb-length length (-> sequence? exact-nonnegative-integer?)]{
A @tech/refer{parameter} that specifies the method for retrieving the
number of @tech{amb tasks} in the current @tech{amb sequence}.
The default value is @racket[queue-length].
}

@defparam[current-amb-pusher push! (-> sequence? continuation? void?)]{
A @tech/refer{parameter} that defines the method for pushing an
@tech{amb task} into the current @tech{amb sequence}.
The default value is @racket[enqueue-front!].
}

@defparam[current-amb-popper pop! (-> sequence? continuation?)]{
A @tech/refer{parameter} that defines the method for popping an
@tech{amb task} from the current @tech{amb sequence}.
The default value is @racket[dequeue!].
}
