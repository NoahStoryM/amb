#lang racket/base

;; ===================================================================
;; The effects of a search
;; ===================================================================
;;
;; The three effects (a choice, failure, a pause), the merger of the
;; choices, and the boundary of a search.  amb/main adds the syntax of
;; choices and the observers; amb/merge has the standard mergers.

(require "private/core.rkt")

(provide make-amb
         amb-generate*
         amb-fail
         amb-pause
         current-amb-merger)
