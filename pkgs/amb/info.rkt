#lang info

(define license 'MIT)
(define collection "amb")
(define version "1.0")

(define pkg-desc "Ambiguous Operator")

(define deps '("base"))
(define build-deps '("scribble-lib" "rackunit-lib" "racket-doc" "data-doc"))

(define scribblings '(("scribblings/amb.scrbl")))

(define clean '("compiled" "private/compiled"))
(define test-omit-paths '(#px"^((?!/tests/).)*$"))
