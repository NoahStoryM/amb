#lang info

(define license 'MIT)
(define collection "typed")
(define version "1.1")

(define pkg-desc "Typed Ambiguous Operator")

(define deps '("base" "typed-racket-lib" ["amb" #:version "1.1"]))
(define build-deps '("rackunit-typed"))
(define implies '("amb"))

(define clean '("compiled" "private/compiled"))
(define test-omit-paths '(#px"^((?!/tests/).)*$"))
