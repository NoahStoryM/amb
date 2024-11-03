#lang info

(define license 'MIT)
(define collection 'multi)
(define version "0.0")

(define pkg-desc "Typed Ambiguous Operator")

(define deps '("base" "amb" "typed-racket-lib" "typed-racket-stream" "typed-data-queue"))
#;(define build-deps '("scribble-lib" "racket-doc" "data-doc"))
(define implies '("amb"))

#;(define scribblings '(("scribblings/amb.scrbl")))

(define clean '("compiled" "private/compiled"))
(define test-omit-paths '(#px"^((?!/tests/).)*$"))
