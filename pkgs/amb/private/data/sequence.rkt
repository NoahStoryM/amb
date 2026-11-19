#lang racket/base

(require (only-in racket/sequence initiate-sequence))
(provide raise-empty-sequence-error
         make-sequence)

(define (raise-empty-sequence-error)
  (raise (exn:fail:contract
          "sequence has no more values"
          (current-continuation-marks))))

;; All traversals share the cursor and its pending answer.  With
;; `advance`, the cursor holds a raw generator.  Clear it before the
;; call so that `advance` sees #f on reentry or after an interrupted
;; read.  Without `advance`, the answer reader handles retries itself.
(define (make-sequence next [advance #f])
  (define pending #f)
  (define done? #f)
  (define (ready? _)
    (or pending
        (and (not done?)
             (let-values ([(answer after)
                           (if advance
                               (advance (begin0 next (set! next #f)))
                               (next))])
               (set! pending answer)
               (set! next after)
               (set! done? (not answer))
               answer))))
  (define (take! pos)
    ;; Another traversal may have taken the answer since our ready? check.
    (unless (or pending (ready? pos))
      (raise-empty-sequence-error))
    (apply values (begin0 pending (set! pending #f))))
  (make-do-sequence
   (λ ()
     (initiate-sequence
      #:init-pos           (void)
      #:continue-with-pos? ready?
      #:pos->element       take!
      #:next-pos           void))))
