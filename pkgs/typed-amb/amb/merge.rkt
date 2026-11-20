#lang typed/racket/base/shallow

;; The typed `amb/merge`: the standard mergers.

(require typed/racket/unsafe
         "private/types.rkt")

(provide Generatorof Merger)

(unsafe-require/typed/provide amb/merge
  [amb-append     Merger]
  [amb-interleave Merger]
  [amb-alternate  Merger]
  [amb-level      Merger])
