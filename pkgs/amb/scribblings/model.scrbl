#lang scribble/manual

@(require (for-label racket/base
                     amb
                     amb/base
                     amb/merge)
          "utils.rkt")

@title[#:tag "model"]{Evaluation Model}

A @deftech{search} runs a computation and reports a sequence of
@deftech{events}.  An event is an @deftech{answer} (the values that the
computation returned), a @deftech{pause}, or the end.  The computation
takes part in the search through three @deftech{effects}:

@itemlist[
@item{A @bold{failure} (@racket[amb-fail]) ends the computation here,
      without an answer.}
@item{A @bold{pause} (@racket[amb-pause]) makes the search report a
      pause.  The computation goes on when the search is asked for its
      next event.}
@item{A @bold{choice} (@racket[make-amb], and the forms built on it:
      @racket[amb], @racket[for/amb], @racket[sequence->amb]) runs the
      rest of the computation once for each @deftech{alternative} that
      is scheduled, and merges the events of those runs into one
      sequence.  A choice among no alternatives, such as @racket[(amb)],
      fails.}
]

How the events of the alternatives are merged is the search strategy.
It belongs to the choice: every choice merges its alternatives with
the @tech{merger} held by @racket[current-amb-merger] where the choice
is made.  The four mergers of @racketmodname[amb/merge] differ in when
they pass the turn from one alternative to the next:

@tabular[#:style 'boxed
         #:sep @hspace[2]
         #:row-properties '(bottom-border ())
         #:column-properties '(top top top)
(list (list @bold{merger} @bold{when the turn passes} @bold{strategy})
      (list @nonbreaking[@racket[amb-append]]
            "after the first alternative ends"
            "depth-first (the default)")
      (list @nonbreaking[@racket[amb-interleave]]
            "at each pause"
            "interleaving, as in miniKanren")
      (list @nonbreaking[@racket[amb-alternate]]
            "at every event, answers included"
            "alternating by event")
      (list @nonbreaking[@racket[amb-level]]
            "after every alternative reaches its next pause"
            "breadth-first by pauses"))]

@amb-examples[
(require amb/merge)
(define (count-from n)
  (amb n (begin (amb-pause) (count-from (add1 n)))))
(for/list ([x (in-amb (list (amb 'a 'b) (count-from 0)))]
           [_ 6])
  x)
(parameterize ([current-amb-merger amb-interleave])
  (for/list ([x (in-amb (list (amb 'a 'b) (count-from 0)))]
             [_ 6])
    x))
]

A search is lazy.  An alternative is not evaluated before it is
chosen, an alternative that a merger has not turned to has not started,
and after an event the search does nothing until it is asked for the
next one.
