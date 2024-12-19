#lang info

(define license 'MIT)
(define collection "typed")
(define version "0.0")

(define pkg-desc "Typed Ambiguous Operator")

(define deps '("base" "amb" "typed-racket-lib"))
(define build-deps '("rackunit-typed"))
(define implies '("amb"))

(define clean '("compiled" "private/compiled"))
(define test-omit-paths '(#px"^((?!/tests/).)*$"))
