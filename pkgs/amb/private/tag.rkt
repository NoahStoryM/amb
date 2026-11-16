#lang racket/base

;; ===================================================================
;; The two values that delimit a search
;; ===================================================================
;;
;; They are not exported by any public module: the effects of
;; private/core.rkt are the only code that uses them.

(provide amb-prompt-tag amb-mark-key)

;; The boundary of a search: every effect is delimited by the nearest
;; prompt with this tag.  All of them are installed by `in-search` of
;; private/core.rkt, with a handler that does not install the prompt
;; again; the effects depend on that.
(define amb-prompt-tag (make-continuation-prompt-tag 'amb))

;; The mark of the code that supplies the alternatives of a choice: an
;; effect that finds it before the boundary of its search is an error.
(define amb-mark-key (make-continuation-mark-key 'amb))
