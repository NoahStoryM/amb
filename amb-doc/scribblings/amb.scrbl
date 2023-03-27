#lang scribble/manual

@(require (for-label racket/base racket/contract amb)
          scribble/example)

@(define (make-amb-eval)
   (make-base-eval #:lang 'racket/base
                   '(require amb)))

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
  (parameterize ([current-amb-tree raise-amb-error])
    (let ([x (for/amb ([i (in-range 10)])
               (displayln i)
               i)])
      (when (< x 5) (amb))))
  (with-handlers ([exn:fail:amb? void])
    (parameterize ([current-amb-tree raise-amb-error])
      (let ([x (for/amb ([i (in-range 10)]) i)])
        (when (even? x) (amb))
        (displayln x)
        (amb))))
  (parameterize ([current-amb-tree raise-amb-error])
    (let-values ([(x y)
                  (for/amb ([v (in-list '([2 9] [9 2]))])
                    (apply values v))])
      (when (> x y) (amb))
      (displayln (list x y)))))
}

@defproc[(make-amb-tree [k continuation?]
                        [alt* (listof (-> any))]
                        [amb-shuffler (current-amb-shuffler) (-> list? list?)]
                        [previous-amb-tree (current-amb-tree) (-> none/c)])
         (-> none/c)]{
The function returns a procedure that represents an amb tree.
When the amb tree is called, it tries one alternative in @racket[alt*] at a time,
at which point it calls @racket[k] with the selected values. The next time amb tree is called,
it tries the next alternative, and so on, until all alternatives have been exhausted.
If all alternatives are exhausted, the amb tree invokes the previous amb tree.

@racket[make-amb-tree] can be used to implement your own amb-style operators,
or to customize the behavior of existing ones by setting @racket[current-amb-tree] to a new amb tree.
}

@defparam[current-amb-tree amb-tree (-> none/c)]{
The parameter detemines the procedure called by @racket[(amb)].
By default, it is @racket[raise-amb-error].
}

@defparam[current-amb-shuffler amb-shuffler (-> list? list?)]{
The parameter returns a shuffler function that can be used to set the order of @racket[alt*].
The default shuffler function simply returns its input list.
}

@defproc[(raise-amb-error) none/c]{
Creates an @racket[exn:fail:amb] value and @racket[raise]s it as
an exception.
}

@defstruct[(exn:fail:amb exn:fail) ()
           #:inspector #f]{
Raised when the amb tree is exhausted.
}
