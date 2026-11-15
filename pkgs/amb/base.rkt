#lang racket/base

;; ===================================================================
;; The effects of a search
;; ===================================================================
;;
;; The three effects (a choice, failure, a pause), the merger of the
;; choices, the boundary of a search, and the mark of the code that
;; supplies the alternatives of a choice.  amb/main adds the syntax of
;; choices and the observers; amb/merge has the standard mergers.

(require "private/core.rkt")

(provide make-amb
         amb-generate*
         amb-fail
         amb-pause
         current-amb-merger
         with-amb-mark
         call-with-amb-mark)
