#lang scribble/manual

@(require (for-label amb) scribble/example)

@(define (make-amb-eval)
   (make-base-eval #:lang 'racket/base
                   '(require amb)))

@(define-syntax-rule (amb-examples body ...)
   (examples #:eval (make-amb-eval) body ...))


@title{amb: Ambiguous Operator}
@defmodule[amb]
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
  (parameterize ([current-amb-tree raise-amb-error])
    (with-handlers ([exn:fail:amb? void])
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

@defparam[current-amb-tree amb-tree (-> any)]{
The parameter detemines the procedure called by @racket[(amb)].
By default, it is @racket[raise-amb-error].
}

@defproc[(raise-amb-error) any]{
Creates an @racket[exn:fail:amb] value and @racket[raise]s it as
an exception.
}

@defstruct[(exn:fail:amb exn:fail) ()
           #:inspector #f]{
Raised when the amb tree is exhausted.
}
