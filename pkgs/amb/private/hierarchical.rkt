#lang racket/base

;; Experimental, single-consumer search protocol. No public API commitment.
;; A step returns #f, an answer, or a suspension. Administrative thunks
;; are NOT suspensions: only explicit suspension affects interleaving.
;; Mature tails are demanded lazily, unlike an eager cons-based oracle.
;; Equivalence therefore excludes divergent mature construction and effects.
(require racket/control
         (submod "amb.rkt" hierarchical-protocol)
         "data/sequence.rkt")

(provide search/thunk search-merge search-bind search-once search-ifte
         search->sequence suspend
         (struct-out answer) (struct-out suspension))

(struct answer (values rest) #:transparent)
(struct suspension (resume) #:transparent)
(struct suspended-continuation (resume))
(define active? (make-parameter #f))
(define (empty-search) #f)

(define (suspend)
  (unless (active?)
    (error 'suspend "requires the experimental hierarchical driver"))
  (shift-at amb-prompt-tag k
    (suspended-continuation k)))

;; Each operand is an administrative thunk producing one protocol event.
;; Preserve the binary grouping; do not flatten or rebalance these nodes.
(define (search-merge left right)
  (lambda ()
    (define event (left))
    (cond
      [(not event) (right)]
      [(answer? event)
       (answer (answer-values event)
               (search-merge (answer-rest event) right))]
      [else
       (suspension
        (search-merge right (suspension-resume event)))])))

(define (search-bind source next)
  (lambda ()
    (define event (source))
    (cond
      [(not event) #f]
      [(answer? event)
       ((search-merge (apply next (answer-values event))
                      (search-bind (answer-rest event) next)))]
      [else
       (suspension (search-bind (suspension-resume event) next))])))

(define (search-once source)
  (lambda ()
    (define event (source))
    (cond
      [(not event) #f]
      [(answer? event) (answer (answer-values event) empty-search)]
      [else (suspension (search-once (suspension-resume event)))])))

(define (search-ifte source then otherwise)
  (lambda ()
    (define event (source))
    (cond
      [(not event) (otherwise)]
      [(answer? event) ((search-bind (lambda () event) then))]
      [else
       (suspension
        (search-ifte (suspension-resume event) then otherwise))])))

;; Existing amb trees enumerate alternatives using mutable cursors. A tree
;; survives a result; a captured suspension continuation runs just once.
(define (tree->search tree)
  (define (step)
    (define event (parameterize ([active? #t]) (tree)))
    (cond
      [(not event) #f]
      [(eq? event #t) (step)]
      [(amb-tree? event) ((search-merge (tree->search event) step))]
      [(suspended-continuation? event)
       ((search-merge
         (lambda ()
           (suspension (continuation->search
                        (suspended-continuation-resume event))))
         step))]
      [else (answer event step)]))
  step)

(define (continuation->search resume)
  (lambda ()
    (define event (parameterize ([active? #t]) (resume (void))))
    (cond
      [(boolean? event) #f]
      [(amb-tree? event) ((tree->search event))]
      [(suspended-continuation? event)
       (suspension (continuation->search
                    (suspended-continuation-resume event)))]
      [else (answer event empty-search)])))

(define (search/thunk thunk)
  (tree->search (make-amb-root thunk)))

(define (search->sequence source)
  (define (next source)
    (lambda ()
      (let loop ([source source])
        (define event (source))
        (cond
          [(not event) (values #f raise-empty-sequence-error)]
          [(answer? event)
           (values (answer-values event) (next (answer-rest event)))]
          [else (loop (suspension-resume event))]))))
  (make-sequence (next source)))
