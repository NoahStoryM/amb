#lang racket/base

(require "../main.rkt"
         (only-in "../private/data/stream.rkt"
                  stream-flatten)
         racket/stream
         rackunit)


(define sequence->amb (compose make-amb sequence-generate))

(define (pythagorean n)
  (for*/stream ([a (in-range 1 n)]
                #:do [(define a² (* a a))]
                [b (in-range a n)]
                #:do [(define b² (* b b))]
                [c (in-range b n)]
                #:do [(define c² (* c c))]
                #:when (= (+ a² b²) c²))
    (vector-immutable a b c)))

(define (amb:pythagorean n)
  (define a (sequence->amb (in-range 1 n)))
  (define a² (* a a))
  (define b (sequence->amb (in-range a n)))
  (define b² (* b b))
  (define c (sequence->amb (in-range b n)))
  (define c² (* c c))
  (unless (= (+ a² b²) c²) (amb))
  (vector-immutable a b c))

(define (stream:pythagorean n)
  (for*/stream ([a (stream-flatten (in-range 1 n))]
                #:do [(define a² (* a a))]
                [b (stream-flatten (in-range a n))]
                #:do [(define b² (* b b))]
                [c (stream-flatten (in-range b n))]
                #:do [(define c² (* c c))]
                #:when (= (+ a² b²) c²))
    (vector-immutable a b c)))

(for ([n '(100 120 140 160 180 200)])
  (printf "pythagorean ~a:\n" n)
  (printf "  built-in: ")
  (define sol*
    (time (for/list ([sol (in-stream (pythagorean n))]) sol)))
  (printf "    stream: ")
  (define stream:sol*
    (time (for/list ([sol (stream-flatten (stream:pythagorean n))]) sol)))
  (check-equal? stream:sol* sol*)
  (printf "       amb: ")
  (define amb:sol*
    (time (for/list ([sol (in-amb (amb:pythagorean n))]) sol)))
  (check-equal? stream:sol* amb:sol*)
  (printf "  ~a solutions\n" (length sol*))
  (newline))
