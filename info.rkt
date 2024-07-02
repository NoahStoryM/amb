#lang info

(define license 'MIT)
(define collection 'multi)
(define version "0.0")

(define pkg-desc "Ambiguous Operator")

(define deps '("base" "typed-racket-lib"))
(define build-deps '("scribble-lib" "racket-doc" "data-doc"))

(define clean '("compiled" "private/compiled"))
(define test-omit-paths '(#px"^((?!/tests/).)*$"))
