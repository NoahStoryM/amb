#lang scribble/manual

@(require (for-label racket/base
                     racket/list
                     racket/function
                     racket/contract
                     data/queue
                     amb)
          "utils.rkt")

@title{amb: Ambiguous Operator}
@defmodule*[(amb typed/amb)]
@author[@author+email["Noah Ma" "noahstorym@gmail.com"]]

@defform*[((amb expr ...)
           (amb : t expr ...))]{
The @racket[amb] operator.
}

@defform[(amb* expr ...)]{
The @racket[amb*] operator evaluates its argument expressions and returns their
results as @racket[values] if the current @tech{amb queue} is empty; otherwise,
it simply runs @racket[(amb)].
}

@deftogether[(@defform*[((for/amb (for-clause ...) body-or-break ... body)
                         (for/amb type-ann-maybe (for-clause ...) type-ann-maybe expr ...+))]
              @defform*[((for*/amb (for-clause ...) body-or-break ... body)
                         (for*/amb type-ann-maybe (for-clause ...) type-ann-maybe expr ...+))])]{
The syntax of @racket[for/amb] and @racket[for*/amb] resembles that of
@racket[for/list] and @racket[for*/list], but instead of evaluating the loop body,
they wrap each iteration as a @racket[thunk] to create @deftech{alternative}s.
This design enables exploration of multiple non-deterministic paths, similar to
@racket[(amb expr ...)].

@amb-examples[
(parameterize ([current-amb-queue (make-queue)])
  (define-values (x y)
    (for/amb ([v '([2 . 9] [9 . 2])])
      (values (car v) (cdr v))))
  (unless (> x y) (amb))
  (displayln (cons x y)))
(parameterize ([current-amb-queue (make-queue)])
  (define x (let next ([i 0]) (amb i (next (add1 i)))))
  (define y (for/amb ([i 3]) i))
  (unless (< x 2) (amb))
  (displayln (cons x y))
  (unless (= (* x y) 2) (amb)))
(parameterize ([current-amb-queue (make-queue)])
  (define-values (x y)
    (for*/amb ([i 3] [j 3])
      (values i j)))
  (when (> x y) (displayln (cons x y)))
  (amb*))
(parameterize ([current-amb-queue (make-queue)])
  (define a* '())
  (define b* '())
  (define x (for/amb ([i 10]) i))
  (when (even? x)
    (set! a* (cons x a*))
    (set! b* (cons (+ x 48) b*)))
  (amb* a* (list->bytes b*)))
]
}

@defstruct[(exn:fail:contract:amb exn:fail:contract) ()
           #:inspector #f]{
Raised when evaluating @racket[(amb)] with an empty @tech{amb queue}.

@amb-examples[
(eval:error (parameterize ([current-amb-queue (make-queue)])
              (amb)))
(eval:error (parameterize ([current-amb-queue (make-queue)])
              (for/amb ([i '()]) i)))
(eval:error (parameterize ([current-amb-queue (make-queue)])
              (for*/amb ([i '()]) i)))
(eval:error
 (parameterize ([current-amb-queue (make-queue)])
   (define-values (x y)
     (for*/amb ([i 3] [j 3])
       (values i j)))
   (unless (> x y) (amb))
   (displayln (cons x y))
   (amb*)))
(eval:error
 (parameterize ([current-amb-queue (make-queue)])
   (define a* '())
   (define b* '())
   (define x (for/amb ([i 10]) i))
   (unless (even? x) (amb))
   (set! a* (cons x a*))
   (set! b* (cons (+ x 48) b*))
   (amb* a* (list->bytes b*))))
]
}

@defproc[(schedule-amb-tasks! [k (-> any/c ... none/c)] [alt* (listof (-> any))]) void?]{
Schedules new @tech{amb tasks} for all @tech{alternatives} in @racket[alt*],
adding them to the current @tech{amb queue}. Each @deftech{amb task} is a
@racket[thunk] that, when invoked, uses @racket[call-in-continuation] to call
@racket[k] with the @racket[values] produced by an @tech{alternative}.
}

@defparam[current-amb-shuffler amb-shuffler (-> list? list?)]{
A @tech/guide{parameter} that specifies how to @racket[shuffle] @racket[alt*]
before scheduling new @tech{amb tasks} into the current @tech{amb queue}. The
default value is @racket[reverse].
}

@defparam[current-amb-queue amb-queue queue?]{
A @tech/guide{parameter} that holds the queue of @tech{amb tasks} to be evaluated,
which is populated as needed by @racket[schedule-amb-tasks!]. The default value
is an empty @deftech{amb queue}.
}

@defparam[current-amb-dequeue! amb-dequeue! (-> queue? (-> none/c))]{
A @tech/guide{parameter} that defines the method for dequeuing an @tech{amb task}
from the current @tech{amb queue}. The default value is @racket[dequeue!], which
removes and returns the @tech{amb task} at the front of the queue.
}

@defparam[current-amb-enqueue! amb-enqueue! (-> queue? (-> none/c) void?)]{
A @tech/guide{parameter} that defines the method for enqueuing an @tech{amb task}
into the current @tech{amb queue}. The default value is @racket[enqueue-front!],
which adds the @tech{amb task} to the front of the queue.
}
