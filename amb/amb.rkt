#lang racket/base

(require "amb/main.rkt"
         racket/contract)

(provide amb
         for/amb for*/amb
         in-amb in-amb*
         (contract-out
          (struct exn:fail:contract:amb
            ([message string?]
             [continuation-marks continuation-mark-set?]))
          [amb* (-> (listof (-> any)) any)]
          #;[in-amb* (-> (-> any) sequence?)]
          [raise-amb-error (-> none/c)]
          [current-amb-empty-handler (parameter/c (-> none/c))]
          [current-amb-shuffler      (parameter/c (-> list? list?))]
          [current-amb-queue         (parameter/c any/c)]
          [current-amb-queue?        (parameter/c (-> any/c boolean?))]
          [current-amb-queue-length  (parameter/c (-> any/c exact-nonnegative-integer?))]
          [current-amb-queue-empty?  (parameter/c (-> any/c boolean?))]
          [current-amb-make-queue    (parameter/c (-> any/c))]
          [current-amb-enqueue!      (parameter/c (-> any/c (-> none/c) void))]
          [current-amb-dequeue!      (parameter/c (-> any/c (-> none/c)))]
          [schedule-amb-tasks!  (-> (listof (-> any)) continuation? void?)]))
