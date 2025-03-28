#lang scribble/manual

@(require (for-label racket/base
                     racket/function
                     racket/contract/base
                     racket/stream
                     racket/mutability
                     syntax/parse
                     data/queue
                     (only-in srfi/43 vector-reverse!)
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

Wrapping @racket[amb] expressions with a new @tech{amb sequence} is recommended.
This ensures that each instance of non-deterministic computation starts with a
fresh @tech/refer{sequence}, avoiding unintended interactions between different
@racket[amb] expressions.

@amb-examples[
(amb 1 (values 2 3) 4)
(amb)
(amb)
(eval:error (amb))
(amb 1 (values 2 3) 4)
(eval:error
 (parameterize ([current-amb-tasks ((current-amb-maker))])
   (let ([x (amb 1 2)])
     (displayln (list x))
     (let ([y (amb 3 4)])
       (displayln (list x y))
       (let ([z (amb 5 6)])
         (displayln (list x y z))
         (amb))))))
(amb)
(eval:error
 (parameterize ([current-amb-shuffler void]
                [current-amb-tasks    ((current-amb-maker))]
                [current-amb-pusher   enqueue!])
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
}

@defproc[(amb* [alt (-> any)] ...) any]{

The backend of @racket[amb].
}

@deftogether[(@defform*[((for/amb maybe-length (for-clause ...) break-clause ... body ...+)
                         (for/amb type-ann-maybe maybe-length (for-clause ...) type-ann-maybe expr ...+))]
              @defform*[((for*/amb maybe-length (for-clause ...) break-clause ... body ...+)
                         (for*/amb type-ann-maybe maybe-length (for-clause ...) type-ann-maybe expr ...+))])]{

The syntax of @racket[for/amb] and @racket[for*/amb] resembles that of
@racket[for/vector] and @racket[for*/vector], but instead of evaluating the loop
body, they wrap each iteration as a @racket[thunk] to create @deftech{alternative}s.

@amb-examples[
(for/amb #:length 4 ([i #(1 2)] [j #(x y)]) (values i j))
(amb)
(amb)
(amb)
(eval:error (amb))
]
}

@section{Stream Constructor}

@defform[(in-amb expr)]{

Constructs a @tech/refer{stream} from the results of evaluating the ambiguous
expression @racket[expr], allowing for lazy evaluation of results.

The @racket[in-amb] form automatically creates a new @tech{amb sequence} and
records all relevant @tech/refer{parameters} at the time @racket[expr] is
evaluated, so there is no need to worry about affecting calls to other
@racket[amb] expressions.

The form @racket[(in-amb expr)] expands to @racket[(in-amb* (λ () expr))].

@amb-examples[
(amb 1 2 3)
(for/list ([(i j) (in-amb (amb (values 1 'x) (values 2 'y)))]) (cons i j))
(amb)
(amb)
(eval:error (amb))
]
}

A good practice is to wrap @racket[amb] expressions in a procedure, then use
@racket[in-amb] or @racket[in-amb*] to create a lazy @tech/refer{stream}
that produces as many results as needed.

@amb-examples[
(define (f n)
  (define m (amb 0 1 2 3 4))
  (unless (> m n) (amb))
  m)
(for/list ([m (in-amb (f 2))]) m)
]

@defproc[(in-amb* [thk (-> any)]) stream?]{

The backend of @racket[in-amb].
}

@amb-examples[
(define (thk)
  (define x (for/amb ([i 10]) i))
  (unless (even? x) (amb))
  x)
(for/list ([x (in-amb* thk)]) x)
]

@section{Exception Type}

@defstruct[(exn:fail:contract:amb exn:fail:contract) ()
           #:inspector #f]{

Raised when evaluating @racket[(amb)] with an empty @tech{amb sequence}.

@amb-examples[
(eval:error
 (parameterize ([current-amb-tasks ((current-amb-maker))])
   (amb)))
(eval:error
 (parameterize ([current-amb-tasks ((current-amb-maker))])
   (amb*)))
(eval:error
 (parameterize ([current-amb-tasks ((current-amb-maker))])
   (for/amb ([i '()]) i)))
(eval:error
 (parameterize ([current-amb-tasks ((current-amb-maker))])
   (for*/amb ([i '()]) i)))
]
}

@defproc[(raise-amb-error) none/c]{
Creates an @racket[exn:fail:contract:amb] value and @racket[raise]s it as an
@tech/guide{exception}.

@amb-examples[
(eval:error
 (raise-amb-error))
]
}

@section{Parameter}

@defparam[current-amb-empty-handler empty-handler (-> none/c)]{

A @tech/refer{parameter} that specifies the procedure to be called when the
@tech{amb sequence} is empty and @racket[(amb)] is evaluated. The default value
is @racket[raise-amb-error].
}

@defparam[current-amb-shuffler shuffle! (-> mutable-vector? void?)]{

A @tech/refer{parameter} that specifies how to shuffle @tech{alternatives} before
scheduling new @tech{amb tasks} into the current @tech{amb sequence}. The
default value is @racket[vector-reverse!].
}

@defparam[current-amb-maker make (-> sequence?)]{

A @tech/refer{parameter} that specifies the method for creating a new
@deftech{amb sequence}. This allows users to define the data structure used to
store @tech{amb tasks}. The default value is @racket[make-queue].
}

@defparam[current-amb-tasks tasks sequence?]{

A @tech/refer{parameter} that holds the @tech/refer{sequence} of @tech{amb tasks}
to be evaluated. Each @deftech{amb task} is a @racket[thunk] that, when invoked,
uses @racket[call-in-continuation] to call an @tech{alternative}. The default
value is @racket[(make-queue)].
}

@defparam[current-amb-length length (-> sequence? exact-nonnegative-integer?)]{

A @tech/refer{parameter} that specifies the method for retrieving the number of
@tech{amb tasks} in the current @tech{amb sequence}. The default value is
@racket[queue-length].
}

@defparam[current-amb-pusher push! (-> sequence? (-> none/c) void?)]{

A @tech/refer{parameter} that defines the method for pushing an @tech{amb task}
into the current @tech{amb sequence}. The default value is @racket[enqueue-front!].
}

@defparam[current-amb-popper pop! (-> sequence? (-> none/c))]{

A @tech/refer{parameter} that defines the method for popping an @tech{amb task}
from the current @tech{amb sequence}. The default value is @racket[dequeue!].
}
