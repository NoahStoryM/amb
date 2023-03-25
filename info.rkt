#lang info

(define license 'MIT)
(define collection 'multi)

(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc"))
(define clean '("compiled" "private/compiled"))

(define test-omit-paths '(#px"^((?!/tests/).)*$"))
