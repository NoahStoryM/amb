#lang typed/racket/base

(require typed/racket/unsafe)

(struct (S T) Queueof ([_ : (Parameter S T)]))
(define-type (Queue S T) (Queueof S T)) ; avoid printing #(struct:Queueof ...)
;; TODO (Queue T) -> (Queue T T)
#;(struct (A ...) _ ([_ : (Parameter A ...)]) #:type-name Queue) ; not work well
(provide Queue)

(unsafe-require/typed/provide
 data/queue
 [make-queue (All (S T) (-> (Queue S T)))]

 [enqueue! (All (S) (-> (Queue S Any) S Void))]
 [enqueue-front! (All (S) (-> (Queue S Any) S Void))]
 [dequeue! (All (T) (-> (Queue Nothing T) T))]

 [queue->list (All (T) (-> (Queue Nothing T) (Listof T)))]
 [queue-length (-> (Queue Nothing Any) Index)]

 [queue? (pred (Queue Nothing Any))]
 [queue-empty? (-> (Queue Nothing Any) Boolean)]
 [non-empty-queue? (All (S T)
                        (case->
                         (-> (Queue S T) Boolean)
                         (-> Any Boolean : #:+ (Queue Nothing Any))))])
