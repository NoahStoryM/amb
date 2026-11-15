#lang typed/racket/base/shallow

;; The typed `amb/base`: the effects of a search.

(require "private/amb.rkt")

(provide Generatorof Generator Merger
         make-amb amb-generate* amb-fail amb-pause
         current-amb-merger
         with-amb-mark call-with-amb-mark)
