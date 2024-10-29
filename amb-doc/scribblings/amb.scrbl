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
results as @racket[values] if the current @racket[amb] queue is empty; otherwise,
it simply runs @racket[(amb)].
}

@deftogether[(@defform*[((for/amb (for-clause ...) body-or-break ... body)
                         (for/amb type-ann-maybe (for-clause ...) type-ann-maybe expr ...+))]
              @defform*[((for*/amb (for-clause ...) body-or-break ... body)
                         (for*/amb type-ann-maybe (for-clause ...) type-ann-maybe expr ...+))])]{
Iterate like @racket[for/list] and @racket[for*/list] respectively, they allow
programmers to explore different possibilities in a non-deterministic way.

@(amb-examples
  (parameterize ([current-amb-queue (make-queue)])
    (let ([x (for/amb ([i (in-range 10)])
               (displayln i)
               i)])
      (when (< x 5) (amb))))
  (with-handlers ([exn:fail:contract? void])
    (parameterize ([current-amb-queue (make-queue)])
      (let ([x (for/amb ([i (in-range 10)]) i)])
        (when (even? x) (amb))
        (displayln x)
        (amb))))
  (parameterize ([current-amb-queue (make-queue)])
    (let-values ([(x y)
                  (for/amb ([v (in-list '([2 9] [9 2]))])
                    (apply values v))])
      (when (> x y) (amb))
      (displayln (list x y)))))
}

@defproc[(insert-amb-node*! [k (-> any/c ... none/c)] [alt* (listof (-> any))]) void?]{
Inserts new @racket[amb] nodes for all @deftech{alternatives} in @racket[alt*]
into the current @racket[amb] queue. An @racket[amb] node is a @racket[thunk]
that calls @racket[k] with the @racket[values] produced by an @tech{alternative}.
}

@defparam[current-amb-shuffler amb-shuffler (-> list? list?)]{
A @tech/guide{parameter} that determines how to @racket[shuffle] @racket[alt*]
before inserting new @racket[amb] nodes into the @racket[amb] queue. The default
value is @racket[reverse].
}

@defparam[current-amb-queue amb-queue queue?]{
A @tech/guide{parameter} that holds the queue of @racket[amb] nodes to be evaluated.
The queue is initially empty and is populated by @racket[insert-amb-node*!].
}

@defparam[current-amb-dequeue! amb-dequeue! (-> queue? (-> none/c))]{
A @tech/guide{parameter} that determines how to dequeue an @racket[amb] node from
the queue. The default value is @racket[dequeue!], which means the node at the
front of the queue is removed and returned.
}

@defparam[current-amb-enqueue! amb-enqueue! (-> queue? (-> none/c) void?)]{
A @tech/guide{parameter} that determines how to enqueue an @racket[amb] node into
the queue. The default value is @racket[enqueue-front!], which means the node is
added to the front of the queue.
}
