#lang racket/base

;; Utilities for the ambiguous operator implementation.
;; These parameters allow customizing how choices are stored and
;; manipulated during search.

(require data/queue)
(provide (all-defined-out))

(define empty-mutable-vector (vector))

;; ----------------------------------------
;; Error definition and helper

(struct exn:fail:contract:amb exn:fail:contract ()
  #:extra-constructor-name make-exn:fail:contract:amb
  #:transparent)

;; Raise a consistent error when `(amb)` runs out of tasks.
(define (raise-amb-error)
  (raise (exn:fail:contract:amb
          "amb: empty amb tasks;\n expected at least one amb task\n  in: (amb)"
          (current-continuation-marks))))

;; Parameters controlling the runtime behaviour of `(amb)`.
(define current-amb-empty-handler (make-parameter raise-amb-error))
;; Called with a vector of choices to shuffle the search order.
(define current-amb-shuffler (make-parameter void))
;; Produce a fresh task container to hold pending tasks.
(define current-amb-maker (make-parameter make-queue))
;; The actual task container used during search.
(define current-amb-tasks (make-parameter (make-queue)))
;; How to determine the number of pending tasks.
(define current-amb-length (make-parameter queue-length))
;; Add and remove tasks from the collection.
(define current-amb-pusher (make-parameter enqueue-front!))
(define current-amb-popper (make-parameter dequeue!))
