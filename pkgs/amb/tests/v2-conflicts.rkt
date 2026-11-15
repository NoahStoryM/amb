#lang racket/base

;; ===================================================================
;; Where the tests of v2 and the semantics of v3 disagree
;; ===================================================================
;;
;; Every entry has the expression of the v2 test (as data: most of them
;; no longer compile), the value that v2.1 gives for it, and a check of
;; what v3 does instead.  The ported test files mark the same places
;; with CONFLICT.  The v2 values of entries 1, 2, 9 and 10 were run on
;; master (6340911); the others are the expectations of the v2 tests.

(require rackunit
         racket/stream
         amb
         amb/merge)

(define-syntax-rule (conflict name #:v2 v2-expr v2-value #:v3 v3-expr v3-value)
  (test-case name
    (void 'v2-expr 'v2-value)
    (check-equal? v3-expr v3-value)
    ;; an entry is a conflict: when the two values are the same, it
    ;; belongs in the ported tests instead
    (check-not-equal? v3-value 'v2-value)))

(define (gone? name) (eq? 'gone (dynamic-require 'amb name (λ () 'gone))))
(define (P) 'P)


;; ---- 1-3: the strategy parameters (tests/amb.rkt, unsafe-amb.rkt, bench.rkt) ----

;; 1. "Test BFS": breadth-first by choices.  v3 has no such strategy:
;; amb-level is breadth-first by pauses, and without pauses it is
;; depth-first.
(conflict "1. current-amb-depth-first?"
  #:v2 (parameterize ([current-amb-depth-first? #f])
         (for/list ([x (in-amb (amb 1 (amb 2 3) (amb 4 5 6) 7 8))]) x))
  (1 7 8 2 3 4 5 6)
  #:v3 (list (gone? 'current-amb-depth-first?)
             (parameterize ([current-amb-merger amb-level])
               (for/list ([x (in-amb (amb 1 (amb 2 3) (amb 4 5 6) 7 8))]) x)))
  '(#t (1 2 3 4 5 6 7 8)))

;; 2. "Test fair search": in v2 the parameter belongs to the tree that
;; is created under it, and decides where that tree goes back in the
;; frontier of the whole search.  In v3 the merger belongs to the choice
;; that is made under it, and merges the alternatives of that choice.
(conflict "2. current-amb-fair? inside a computation"
  #:v2 (for/list ([i (in-amb (let* ([a '(0)]
                                    [b (append a (list (amb 1 2 3)))])
                               (parameterize ([current-amb-fair? #t])
                                 (append b (list (amb 4 5 6))))))])
         i)
  ((0 1 4) (0 2 4) (0 3 4) (0 1 5) (0 2 5) (0 3 5) (0 1 6) (0 2 6) (0 3 6))
  #:v3 (list (gone? 'current-amb-fair?)
             (for/list ([i (in-amb (let* ([a '(0)]
                                          [b (append a (list (amb 1 2 3)))])
                                     (parameterize ([current-amb-merger amb-alternate])
                                       (append b (list (amb 4 5 6))))))])
               i))
  '(#t ((0 1 4) (0 1 5) (0 1 6) (0 2 4) (0 2 5) (0 2 6) (0 3 4) (0 3 5) (0 3 6))))

;; 3. The four combinations of the two parameters, and the four mergers.
;; On these two programs (no pauses) depth-first is amb-append and
;; depth-first + fair is amb-alternate; the two breadth-first ones have
;; no counterpart.  The ported tests (unsafe-amb.rkt P8, P9; bench.rkt;
;; amb-handler.rkt "every strategy") use the four mergers and compare
;; sets of answers only.
(conflict "3. the four strategies"
  #:v2 (for*/list ([df '(#t #f)] [fair '(#t #f)])
         (parameterize ([current-amb-depth-first? df] [current-amb-fair? fair])
           (for/list ([x (in-amb (amb 1 (amb 2 3) (amb 4 5 6) 7 8))]) x)))
  ((1 2 4 3 7 5 8 6) (1 2 3 4 5 6 7 8) (1 2 7 4 3 8 5 6) (1 7 8 2 3 4 5 6))
  #:v3 (for/list ([m (list amb-alternate amb-append amb-interleave amb-level)])
         (parameterize ([current-amb-merger m])
           (for/list ([x (in-amb (amb 1 (amb 2 3) (amb 4 5 6) 7 8))]) x)))
  '((1 2 4 3 7 5 8 6) (1 2 3 4 5 6 7 8) (1 2 3 4 5 6 7 8) (1 2 3 4 5 6 7 8)))


;; ---- 4-8: (amb) and the empty handler (tests/amb-handler.rkt) ----

;; 4. "with #:empty-handler, (amb) outputs the handler's values and
;; abandons the rest".  v3: (amb) fails; a pause is (amb-pause), and it
;; does not abandon the rest.
(conflict "4. (amb) is answered by the empty handler"
  #:v2 (for/list ([x (in-amb/do (begin (amb) 'late) #:empty-handler P)]) x)
  (P)
  #:v3 (list (for/list ([x (in-amb/do (begin (amb) 'late) #:on-pause P)]) x)
             (for/list ([x (in-amb/do (begin (amb-pause) 'late) #:on-pause P)]) x))
  '(() (P late)))

;; 5. "a choice with no alternatives is (amb)", "#:length: only the
;; default fill counts as no alternative", "(amb) in a for/amb body".
(conflict "5. a choice with no alternatives is answered by the empty handler"
  #:v2 (list (for/list ([x (in-amb/do (for/amb ([i '()]) i) #:empty-handler P)]) x)
             (for/list ([x (in-amb/do (for/amb #:length 5 ([i '()]) i) #:empty-handler P)]) x)
             (stream->list (in-amb (for/amb ([i '(1 2 3)]) (if (odd? i) (amb) i)) #:empty-handler P)))
  ((P) (P) (P 2 P))
  #:v3 (list (for/list ([x (in-amb/do (for/amb ([i '()]) i) #:on-pause P)]) x)
             (for/list ([x (in-amb/do (for/amb #:length 5 ([i '()]) i) #:on-pause P)]) x)
             (stream->list (in-amb (for/amb ([i '(1 2 3)]) (if (odd? i) (amb) i)) #:on-pause P)))
  '(() () (2)))

;; 6. "the handler runs where (amb) is called".  v3: the handler of
;; #:on-pause is called by whoever observes the search, outside it.
(conflict "6. the handler runs in the search"
  #:v2 (let ([p (make-parameter 'outside)])
         (for/list ([x (in-amb/do (parameterize ([p 'inside]) (amb)) #:empty-handler p)]) x))
  (inside)
  #:v3 (let ([p (make-parameter 'outside)])
         (for/list ([x (in-amb/do (parameterize ([p 'inside]) (amb-pause) 'x) #:on-pause p)]) x))
  '(outside x))

;; 7. "function forms take the handler as an optional argument".  v3: by
;; keyword.
(conflict "7. the handler of in-amb* and in-amb*/do is an optional argument"
  #:v2 (for/list ([x (in-amb*/do (λ () (when (amb #t #f) (amb)) 'x) P)]) x)
  (P x)
  #:v3 (list (with-handlers ([exn:fail:contract? (λ (e) 'arity-error)])
               (for/list ([x ((dynamic-require 'amb 'in-amb*/do) (λ () (amb-pause) 'x) P)]) x))
             (for/list ([x (in-amb*/do (λ () (amb-pause) 'x) #:on-pause P)]) x))
  '(arity-error (P x)))

;; 8. "(amb) inside the handler of the same search is an error; other
;; choices are fine", "reentry is detected through empty choices and
;; resumed continuations too", "after a reentry error the search goes
;; on".  v3 has no empty handler; an effect in the handler of #:on-pause
;; is an effect of the search around the observer, if there is one.
(conflict "8. reentry into the empty handler"
  #:v2 (with-handlers ([exn:fail? (λ (e) 'reentry-error)])
         (stream->list (in-amb (amb) #:empty-handler (λ () (amb)))))
  reentry-error
  #:v3 (stream->list (in-amb (stream->list (in-amb (amb-pause) #:on-pause (λ () (amb 1 2))))))
  (list (list 1 (void)) (list 2 (void))))


;; ---- 9: the shuffler (tests/amb-handler.rkt) ----

;; 9. "a choice with no alternative does not call the shuffler".  v3:
;; once for every vector of alternatives (V3-PLAN section 7).
(conflict "9. the shuffler and the empty vector"
  #:v2 (let ([calls 0])
         (parameterize ([current-amb-shuffler
                         (λ (v) (when (zero? (vector-length v)) (set! calls (add1 calls))))])
           (for ([x (in-amb/do (amb))]) x)
           (for ([x (in-amb/do (for/amb #:length 0 ([i '()]) i))]) x))
         calls)
  0
  #:v3 (let ([calls 0])
         (parameterize ([current-amb-shuffler
                         (λ (v) (when (zero? (vector-length v)) (set! calls (add1 calls))))])
           (for ([x (in-amb/do (amb))]) x)
           (for ([x (in-amb/do (for/amb #:length 0 ([i '()]) i))]) x))
         calls)
  2)


;; ---- 10: infinitely many alternatives (tests/bench.rkt, section 5) ----

;; 10. With `current-amb-fair?`, v2 finds a Pythagorean triple among
;; three nested choices over the naturals, in a program without pauses
;; (depth-first + fair and breadth-first + fair; the other two diverge).
;; In v3 no standard merger passes the turn on where nothing pauses, so
;; the same program diverges with all four (not run here).  With a pause
;; in every alternative, amb-interleave and amb-alternate find one.
(define (pythagorean pause)
  (define a (for/amb ([n (in-naturals 1)]) (pause) n))
  (define b (for/amb ([n (in-naturals 1)]) (pause) n))
  (define c (for/amb ([n (in-naturals 1)]) (pause) n))
  (unless (= (+ (* a a) (* b b)) (* c c)) (amb))
  (list a b c))
(conflict "10. a fair search without pauses"
  #:v2 (parameterize ([current-amb-fair? #t])
         (stream-first (in-amb (pythagorean void))))
  (4 3 5)
  #:v3 (for/list ([m (list amb-interleave amb-alternate)])
         (parameterize ([current-amb-merger m])
           (stream-first (in-amb (pythagorean amb-pause)))))
  '((3 4 5) (3 4 5)))


;; ---- 11-12: make-amb and the private interface ----

;; 11. tests/amb.rkt "Test sequence->amb", tests/pythagorean.rkt, and
;; "make-amb does not call more? again after it returned #f".  v3:
;; `make-amb` takes one generator, in the shape of `sequence-generate*`.
(conflict "11. make-amb takes more? and get"
  #:v2 (for/list ([x (in-amb ((compose make-amb sequence-generate) '(1 2 3)))]) x)
  (1 2 3)
  #:v3 (list (with-handlers ([exn:fail:contract? (λ (e) 'arity-error)])
               (for/list ([x (in-amb ((compose make-amb sequence-generate) '(1 2 3)))]) x))
             (for/list ([x (in-amb (make-amb (λ () (sequence-generate* '(1 2 3)))))]) x))
  '(arity-error (1 2 3)))

;; 12. tests/unsafe-amb.rkt P7b, and its sections 4-7 on the compaction
;; of the vector.  v3: there is no amb/private/amb.
(conflict "12. unsafe-amb* and thunks"
  #:v2 (stream->list (in-amb (amb ((dynamic-require 'amb/private/amb 'unsafe-amb*) (vector)) 9)))
  (9)
  #:v3 (with-handlers ([exn:fail? (λ (e) 'no-such-module)])
         (dynamic-require 'amb/private/amb 'unsafe-amb*))
  'no-such-module)
