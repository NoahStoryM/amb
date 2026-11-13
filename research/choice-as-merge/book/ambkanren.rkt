#lang typed/racket/base

;; The examples of the book on ambKanren's typed/miniKanren/amb/trs2, as
;; installed.  NAME names the record file: results/ambkanren.rktd is from
;; its master branch, results/ambkanren-lawful.rktd from lawful-search.

(require racket/include
         (except-in racket/match ==)
         (only-in racket/list make-list)
         (for-syntax racket/base racket/path)
         typed/miniKanren/amb/trs2)

(require/typed "record.rkt"
  [record! (→ String (→ Any) Any Void)]
  [finish! (→ String (Option Path-String) Void)])

(include "examples.rktl")

(define name (or (getenv "NAME") "ambkanren"))
(finish! name (build-path "results" (string-append name ".rktd")))
