#lang typed/racket/base/shallow

;; The typed `amb/merge`: the standard mergers.

(require "private/amb.rkt")

(provide Generatorof Merger
         amb-append amb-interleave amb-alternate amb-level)
