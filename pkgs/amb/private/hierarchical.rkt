#lang racket/base

;; Experimental, single-consumer search protocol. No public API commitment.
;; A step returns #f, an answer, or a suspension. Administrative thunks
;; are NOT suspensions: only explicit suspension affects interleaving.
;; Mature tails are demanded lazily, unlike an eager cons-based oracle.
;; Equivalence therefore excludes divergent mature construction and effects.
(require racket/control
         (submod "amb.rkt" hierarchical-protocol)
         "data/sequence.rkt")

(provide search/thunk search-defer search-delay
         search-merge search-bind search-once search-ifte
         search->sequence make-hierarchical-sequence
         suspend call-with-local-search call-with-local-default
         (struct-out answer) (struct-out suspension))

(struct answer (values rest) #:transparent)
(struct suspension (resume) #:transparent)
(struct suspended-continuation (resume))
(struct local-search (source))
(define engine-key (make-continuation-mark-key 'amb-engine))
(define (empty-search) #f)

;; Administrative laziness does not give siblings a scheduling turn.
(define (search-defer resume)
  (lambda () ((resume))))

;; An interpreter that already has a local resumption need not capture and
;; immediately bind a throwaway continuation merely to introduce a pause.
(define (search-delay resume)
  (lambda () (suspension (search-defer resume))))

(define (check-active who)
  ;; Inspect only the nearest amb prompt. A nested flat root must not inherit
  ;; the outer root's marker, and flat roots need no extra dynamic binding.
  (unless (and (continuation-prompt-available? amb-prompt-tag)
               (eq? (continuation-mark-set-first
                     #f engine-key #f amb-prompt-tag)
                    'hierarchical))
    (error who "requires the experimental hierarchical driver")))

(define (suspend)
  (check-active 'amb-suspend)
  (shift-at amb-prompt-tag k
    (suspended-continuation k)))

;; Capture the caller separately from the local body. The caller resumes in
;; its original typed continuation; no state-accepting FFI callback is needed.
(define (check-limit limit)
  (unless (or (not limit) (exact-nonnegative-integer? limit))
    (raise-argument-error 'amb-local "(or/c #f exact-nonnegative-integer?)" limit)))

(struct local-result (values))
(define start-body (gensym 'amb-local-body))
(define start-default (gensym 'amb-local-default))

;; Enter the body through K, so its parameters, continuation marks, and wind
;; frames are the call site's. Abort at the local boundary before the rest
;; of K runs. Answers later resume that same K through a different action.
(define (call-with-local-search* thunk limit default)
  (check-active 'amb-local)
  (check-limit limit)
  (define action
    (shift-at amb-prompt-tag k
      (define (resume action)
        (continuation->search (lambda (_) (k action))))
      (define (continue . results) (resume (local-result results)))
      (define source (resume start-body))
      (define limited (if limit (search-limit source limit) source))
      (local-search
       (if default
           (search-ifte limited continue
                        (search-bind (resume start-default) continue))
           (search-bind limited continue)))))
  (cond
    [(or (eq? action start-body) (eq? action start-default))
     (call-with-values (if (eq? action start-body) thunk default)
       (lambda results
         (abort/cc amb-prompt-tag (lambda () results))))]
    [else (apply values (local-result-values action))]))

(define (call-with-local-search thunk [limit #f])
  (call-with-local-search* thunk limit #f))

;; Once SOURCE has answered, failures in the caller cannot select DEFAULT.
(define (call-with-local-default thunk default [limit #f])
  (call-with-local-search* thunk limit default))

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

(define (search-limit source count)
  (lambda ()
    (and (positive? count)
         (let ([event (source)])
           (cond
             [(not event) #f]
             [(answer? event)
              (answer (answer-values event)
                      (search-limit (answer-rest event) (sub1 count)))]
             [else (suspension (search-limit (suspension-resume event) count))])))))

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
    (define event (tree))
    (cond
      [(not event) #f]
      [(eq? event #t) (step)]
      [(amb-tree? event) ((search-merge (tree->search event) step))]
      [(local-search? event) ((search-merge (local-search-source event) step))]
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
    (define event (resume (void)))
    (cond
      [(boolean? event) #f]
      [(amb-tree? event) ((tree->search event))]
      [(local-search? event) ((local-search-source event))]
      [(suspended-continuation? event)
       (suspension (continuation->search
                    (suspended-continuation-resume event)))]
      [else (answer event empty-search)])))

(define (search/thunk thunk)
  ;; A synthetic root runs once; unlike a choice cursor, it has no further
  ;; alternatives to merge after its result. Drop that empty administrative
  ;; tail without introducing or removing any suspension event.
  (define root
    (make-amb-root
     (lambda ()
       (with-continuation-mark engine-key 'hierarchical
         (thunk)))))
  (continuation->search (lambda (_) (root))))

(define (make-hierarchical-sequence thunk make-sequence exhausted)
  (search->sequence (search/thunk thunk) make-sequence exhausted))

(define (search->sequence source [make-result-sequence make-sequence]
                          [exhausted raise-empty-sequence-error])
  (define (next source)
    (lambda ()
      (let loop ([source source])
        (define event (source))
        (cond
          [(not event) (values #f exhausted)]
          [(answer? event)
           (values (answer-values event) (next (answer-rest event)))]
          [else (loop (suspension-resume event))]))))
  (make-result-sequence (next source)))
