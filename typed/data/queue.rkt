#lang typed/racket/base

(require "queue/main.rkt")
(provide (all-from-out "queue/main.rkt")
         (rename-out [Queue Queueof]))
