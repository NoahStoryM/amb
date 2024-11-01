#lang typed/racket/base

(require typed/racket/unsafe)

(struct (S T) Queueof ([_ : (Parameter S T)]))
(define-type (Queue S T) (Queueof S T)) ; avoid printing #(struct:Queueof ...)
(define-type QueueTop (Queue Nothing Any))
(define-type QueueBot (Queue Any Nothing))
;; TODO (Queue T) -> (Queue T T)
#;(struct (A ...) _ ([_ : (Parameter A ...)]) #:type-name Queue) ; not work well
(provide Queue QueueTop QueueBot)

(unsafe-require/typed/provide
 data/queue
 [make-queue (∀ (S T) (-> (Queue S T)))]

 [enqueue! (∀ (S) (-> (Queue S Any) S Void))]
 [enqueue-front! (∀ (S) (-> (Queue S Any) S Void))]
 [dequeue! (∀ (T) (-> (Queue Nothing T) T))]

 [queue->list (∀ (T) (-> (Queue Nothing T) (Listof T)))]
 [queue-length (-> QueueTop Index)]

 [queue? (pred QueueTop)]
 [queue-empty? (-> QueueTop Boolean)]
 [non-empty-queue? (∀ (S T)
                      (case->
                       (-> (Queue S T) Boolean)
                       (-> Any Boolean : #:+ QueueTop)))]

 [in-queue (∀ (T) (-> (Queue Nothing T) (Sequenceof T)))])
