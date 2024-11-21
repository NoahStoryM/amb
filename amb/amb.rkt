#lang racket/base

(require "amb/main.rkt"
         racket/contract
         data/queue)

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
          [current-amb-shuffler (parameter/c (-> list? list?))]
          [current-amb-queue    (parameter/c queue?)]
          [current-amb-enqueue! (parameter/c (-> queue? (-> none/c) void?))]
          [current-amb-dequeue! (parameter/c (-> queue? (-> none/c)))]
          [schedule-amb-tasks!  (-> (listof (-> any)) continuation? void?)]))
