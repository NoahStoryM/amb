#lang scribble/manual

@(require (for-label racket/base
                     racket/function
                     racket/contract
                     data/queue
                     amb)
          scribble/example)

@(define (make-amb-eval)
   (make-base-eval #:lang 'racket/base
                   '(require data/queue amb)))

@(define-syntax-rule (amb-examples body ...)
   (examples #:eval (make-amb-eval) body ...))


@title{amb: Ambiguous Operator}
@defmodule*[(amb typed/amb)]
@author[@author+email["Noah Ma" "noahstorym@gmail.com"]]

@defform[(amb expr ...)]{
The amb operator.
}

@deftogether[(@defform[(for/amb (for-clause ...) body-or-break ... body)]
              @defform[(for*/amb (for-clause ...) body-or-break ... body)])]{
Iterate like @racket[for/list] and @racket[for*/list] respectively,
they allow programmers to explore different possibilities in a non-deterministic way.

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
Inserts new amb nodes for all alternatives in @racket[alt*] into the current amb queue.
An amb node is a @racket[thunk] that calls @racket[k] with the values produced by an alternative.
}

@defparam[current-amb-shuffler amb-shuffler (-> list? list?)]{
A parameter that determines how to shuffle the alternatives before inserting them into the amb queue.
The default value is @racket[reverse].
}

@defparam[current-amb-queue amb-queue queue?]{
A parameter that holds the queue of amb nodes to be evaluated.
The queue is initially empty and is populated by @racket[insert-amb-node*!].
}

@defparam[current-amb-dequeue! amb-dequeue! (-> queue? (-> none/c))]{
A parameter that determines how to dequeue an amb node from the queue.
The default value is @racket[dequeue!], which means the node at the front of the queue is removed and returned.
}

@defparam[current-amb-enqueue! amb-enqueue! (-> queue? (-> none/c) void?)]{
A parameter that determines how to enqueue an amb node into the queue.
The default value is @racket[enqueue-front!], which means the node is added to the front of the queue.
}
