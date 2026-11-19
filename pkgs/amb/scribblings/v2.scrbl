#lang scribble/manual

@(require (for-label racket/base
                     amb
                     amb/base
                     amb/merge)
          "utils.rkt")

@title[#:tag "v2"]{Changes from Version 2}

Version 3 is a new design.  A search is no longer a tree whose
frontier a driver schedules, but a merge of generators, and the
strategy is a value that belongs to each choice.  Programs that only
use @racket[amb], @racket[for/amb], @racket[amb-fail] and
@racket[in-amb] with the default strategy behave as before.

@bold{The most visible change.}  In version 2,
@racket[current-amb-fair?] made a search complete over infinitely many
alternatives without any change to the program.  In version 3,
fairness comes from the pauses of the program (@secref["fairness"]),
so such a program, moved to version 3 unchanged, does not terminate.
To port it, choose @racket[amb-interleave], and put an
@racket[(amb-pause)] in every alternative of an infinite choice and at
the entry of every recursive procedure that can recur forever without
an answer.

The differences, one by one:

@tabular[#:style 'boxed
         #:sep @hspace[2]
         #:row-properties '(bottom-border ())
         #:column-properties '(top top)
(list (list @bold{Version 2} @bold{Version 3})
      (list @para{@racket[current-amb-depth-first?] set to @racket[#f] gave breadth-first search by choices.}
            @para{The parameter is removed.  @racket[amb-level] is breadth-first by pauses; in a program without pauses it is depth-first.})
      (list @para{@racket[current-amb-fair?], set inside a computation, decided where the tree went back into the frontier of the whole search.}
            @para{The parameter is removed.  @racket[current-amb-merger] belongs to the choice made under it, and merges the alternatives of that choice only.})
      (list @para{The two parameters gave four strategies.}
            @para{There are four mergers, which are different strategies.  On programs without pauses, depth-first is @racket[amb-append], and depth-first with fairness is @racket[amb-alternate]; the two breadth-first strategies have no counterpart.})
      (list @para{@racket[(amb)] was answered by the handler of @racketidfont{#:empty-handler}, and abandoned the rest of the computation.  A pause was written @racket[(when (amb #t #f) (amb))].}
            @para{@racket[(amb)] fails.  A pause is @racket[(amb-pause)]; the computation goes on after it, and the observer sees it through @racket[#:on-pause].})
      (list @para{A choice with no alternatives, such as an empty @racket[for/amb], was answered by that handler too.}
            @para{It fails.})
      (list @para{The handler ran inside the search, where @racket[(amb)] was called.  Reaching @racket[(amb)] of the same search from the handler was a reentry error.}
            @para{The handler of @racket[#:on-pause] is called by the observer, outside the search.  An effect in it belongs to the search around the observer.})
      (list @para{The shuffler was not called for a choice with no alternatives.}
            @para{It is called once for every vector of alternatives, the empty one too.})
      (list @para{A fair search needed no pauses.}
            @para{It needs them; see above.})
      (list @para{@racket[make-amb] took two procedures, @racket[_more?] and @racket[_get].}
            @para{It takes one @tech{generator}.  @racket[sequence->amb] is exported.})
      (list @para{@racketidfont{amb/private/amb} exported @racketidfont{unsafe-amb*}.}
            @para{The module is removed.})
      (list @para{A choice or a failure in a clause of @racket[for/amb] did something that depended on the implementation.}
            @para{It is an error (@secref["supply"]).})
      (list @para{After an exception in the search, reading on ran the same part of the search again.}
            @para{Reading on is an error (@secref["exceptions"]).}))]
