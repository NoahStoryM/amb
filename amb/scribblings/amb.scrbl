#lang scribble/manual

@(require (for-label racket/base
                     racket/list
                     racket/function
                     racket/contract
                     racket/mutable-treelist
                     syntax/parse
                     amb)
          "utils.rkt")

@title{amb: Ambiguous Operator}
@defmodule[amb       #:packages ("amb")]
@defmodule[typed/amb #:packages ("typed-amb") #:no-declare]
@author[@author+email["Noah Ma" "noahstorym@gmail.com"]]

@section{Ambiguous Operator}

@defform[(amb expr ...)]{

John McCarthy's ambiguous operator.

The form @racket[(amb expr ...)] expands to @racket[(amb* (list (λ () expr) ...))].

Wrapping @racket[amb] expressions with a new @tech{amb mtreelist} is recommended.
This ensures that each instance of non-deterministic computation starts with a
fresh mtreelist, avoiding unintended interactions between different @racket[amb]
expressions.

@amb-examples[
(eval:error
 (parameterize ([current-amb-tasks (mutable-treelist)])
   (let ([x (amb 1 2)])
     (displayln (list x))
     (let ([y (amb 3 4)])
       (displayln (list x y))
       (let ([z (amb 5 6)])
         (displayln (list x y z))
         (amb))))))
(eval:error
 (parameterize ([current-amb-shuffler values]
                [current-amb-tasks    (mutable-treelist)]
                [current-amb-pusher   mutable-treelist-cons!])
   (let ([x (amb 1 2)])
     (displayln (list x))
     (let ([y (amb 3 4)])
       (displayln (list x y))
       (let ([z (amb 5 6)])
         (displayln (list x y z))
         (amb))))))
]
}

@defproc[(amb* [alt* (listof (-> any))]) any]{

A helper procedure used by @racket[amb].
}

@deftogether[(@defform*[((for/amb (for-clause ...) break-clause ... body ...+)
                         (for/amb type-ann-maybe (for-clause ...) type-ann-maybe expr ...+))]
              @defform*[((for*/amb (for-clause ...) break-clause ... body ...+)
                         (for*/amb type-ann-maybe (for-clause ...) type-ann-maybe expr ...+))])]{

The syntax of @racket[for/amb] and @racket[for*/amb] resembles that of
@racket[for/list] and @racket[for*/list], but instead of evaluating the loop body,
they wrap each iteration as a @racket[thunk] to create @deftech{alternative}s.

The form @racket[(for/amb (for-clause ...) break-clause ... body ...+)] expands
to @racket[(amb* (for/list (for-clause ...) break-clause ... (λ () body ...+)))].

@amb-examples[
(require racket/list)
(parameterize ([current-amb-shuffler shuffle]
               [current-amb-tasks    (mutable-treelist)]
               [current-amb-pusher   mutable-treelist-cons!])
  (define x (let next ([i 0]) (amb (next (add1 i)) i)))
  (define y (for/amb ([i 3]) i))
  (unless (< x 2) (amb))
  (displayln (cons x y))
  (unless (= (+ x y) 3) (amb)))
(parameterize ([current-amb-tasks (mutable-treelist)])
  (define-values (x y)
    (for/amb ([v '([2 . 9] [9 . 2])])
      (values (car v) (cdr v))))
  (unless (> x y) (amb))
  (displayln (cons x y)))
(let/cc break
  (parameterize ([current-amb-tasks (mutable-treelist)]
                 [current-amb-empty-handler break])
    (define-values (x y)
      (for*/amb ([i 3] [j 3])
        (values i j)))
    (unless (> x y) (amb))
    (displayln (cons x y))
    (amb)))
(let/cc k
  (define a* '())
  (define b* '())
  (define (return) (k a* (list->bytes b*)))
  (parameterize ([current-amb-tasks (mutable-treelist)]
                 [current-amb-empty-handler return])
    (define x (for/amb ([i 10]) i))
    (when (even? x)
      (set! a* (cons x a*))
      (set! b* (cons (+ x 48) b*)))
    (amb)))
]
}

@section{Sequence Constructor}

@defform[(in-amb expr)]{

Constructs a @tech/refer{sequence} from the results of evaluating the ambiguous
expression @racket[expr], allowing for lazy evaluation of results. The
@racket[in-amb] form automatically creates a new @tech{amb mtreelist}, so there is no
need to worry about affecting calls to other @racket[amb] expressions.

The form @racket[(in-amb expr)] expands to @racket[(in-amb* (λ () expr))].

@amb-examples[
(parameterize ([current-amb-tasks (mutable-treelist)])
  (define (next i j) (amb (values i j) (next (add1 i) (sub1 j))))
  (amb 1 2 3)
  (displayln (= 2 (mutable-treelist-length (current-amb-tasks))))
  (time
   (displayln
    (for/and ([i (in-range 100000)]
              [(j k) (in-amb (next 0 0))])
      (= i j (- k)))))
  (displayln (= 2 (mutable-treelist-length (current-amb-tasks))))
  )
]
}

A good practice is to wrap @racket[amb] expressions in a procedure, then use
@racket[in-amb] or @racket[in-amb*] to create a lazy @tech/refer{sequence}
that produces as many results as needed.

@amb-examples[
(define (f n)
  (define m (amb 0 1 2 3 4))
  (unless (> m n) (amb))
  m)
(for ([m (in-amb (f 2))])
  (displayln m))
]

@defproc[(in-amb* [thk (-> any)]) sequence?]{

A helper procedure used by @racket[in-amb].
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

Raised when evaluating @racket[(amb)] with an empty @tech{amb mtreelist}.

@amb-examples[
(eval:error
 (parameterize ([current-amb-tasks (mutable-treelist)])
   (amb)))
(eval:error
 (parameterize ([current-amb-tasks (mutable-treelist)])
   (amb* '())))
(eval:error
 (parameterize ([current-amb-tasks (mutable-treelist)])
   (for/amb ([i '()]) i)))
(eval:error
 (parameterize ([current-amb-tasks (mutable-treelist)])
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

@section{Amb Tasks Management}

@defproc[(schedule-amb-tasks!
          [k continuation?]
          [alt* (listof (-> any))]
          [amb-tasks mutable-treelist? (current-amb-tasks)])
         void?]{

Schedules new @tech{amb tasks} for all @tech{alternatives} in @racket[alt*],
adding them to @racket[amb-tasks]. Each @tech{amb task} is a @racket[thunk] that,
when invoked, uses @racket[call-in-continuation] to call an @tech{alternative}
in @racket[k].
}

@section{Parameter}

@defparam[current-amb-empty-handler amb-empty-handler (-> none/c)]{

A @tech/refer{parameter} that specifies the procedure to be called when the
@tech{amb mtreelist} is empty and @racket[(amb)] is evaluated. The default value is
@racket[raise-amb-error].
}

@defparam[current-amb-shuffler amb-shuffler (-> list? list?)]{

A @tech/refer{parameter} that specifies how to @racket[shuffle] @racket[alt*]
before scheduling new @tech{amb tasks} into the current @tech{amb mtreelist}. The
default value is @racket[reverse].
}

@defparam[current-amb-tasks tasks mutable-treelist?]{

A @tech/refer{parameter} that holds the mtreelist of @deftech{amb task}s to be
evaluated, which is populated as needed by @racket[schedule-amb-tasks!]. The
default value is an empty @deftech{amb mtreelist}.
}

@defparam[current-amb-popper pop! (-> mutable-treelist? (-> none/c))]{

A @tech/refer{parameter} that defines the method for dequeuing an @tech{amb task}
from the current @tech{amb mtreelist}. The default value removes and returns the
@tech{amb task} at the end of the mtreelist.
}

@defparam[current-amb-pusher push! (-> mutable-treelist? (-> none/c) void?)]{

A @tech/refer{parameter} that defines the method for enqueuing an @tech{amb task}
into the current @tech{amb mtreelist}. The default value adds the @tech{amb task}
to the end of the queue.
}
