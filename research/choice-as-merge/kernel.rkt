#lang racket/base

;; The kernel: kept at this path for the tests, the book and the
;; benchmark; the code is in amb/main.rkt.

(require "amb/main.rkt")
(provide (all-from-out "amb/main.rkt"))
