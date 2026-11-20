#lang racket/base

;; Forward the typed bindings so this entry point can be combined with
;; typed/amb/base and typed/amb/merge.
(require "amb/main.rkt")
(provide (all-from-out "amb/main.rkt"))
