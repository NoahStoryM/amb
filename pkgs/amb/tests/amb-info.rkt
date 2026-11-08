#lang racket/base

;; Experimental coverage for optional per-node choice metadata.
;; Run with: raco test pkgs/amb/tests/amb-info.rkt
(require rackunit racket/stream amb)

(define (tagged-choice tag candidates)
  (define pending candidates)
  (make-amb
   (λ () (pair? pending))
   (λ ()
     (define value (car pending))
     (set! pending (cdr pending))
     (list value (current-amb-info)))
   tag))

(test-case "backwards-compatible default info"
  (check-equal?
   (stream->list
    (in-amb
     (let ([pending '(10 20)])
       (make-amb
        (λ () (pair? pending))
        (λ ()
          (define value (car pending))
          (set! pending (cdr pending))
          (list value (current-amb-info)))))))
   '((10 #f) (20 #f))))

(test-case "metadata installed for every generator resume"
  (check-equal?
   (stream->list (in-amb (tagged-choice 'left '(1 2 3))))
   '((1 left) (2 left) (3 left))))

(test-case "ordinary amb remains unchanged"
  (check-equal? (stream->list (in-amb (amb 1 2 3))) '(1 2 3)))

(test-case "nested choices carry their own metadata"
  (check-equal?
   (stream->list
    (in-amb
     (let ([a (tagged-choice 'outer '(a b))])
       (list a (tagged-choice 'inner '(1 2))))))
   '(((a outer) (1 inner))
     ((a outer) (2 inner))
     ((b outer) (1 inner))
     ((b outer) (2 inner)))))

(test-case "interleaved nodes do not overwrite each other's info"
  (check-equal?
   (parameterize ([current-amb-fair? #t])
     (stream->list
      (in-amb
       (amb (tagged-choice 'left '(1 2 3))
            (tagged-choice 'right '(10 20 30))))))
   '((1 left) (10 right) (2 left) (20 right) (3 left) (30 right))))

(test-case "info binding is restored after each node returns"
  (parameterize ([current-amb-info 'caller])
    (define st (in-amb (tagged-choice 'node '(1 2))))
    (check-equal? (stream-first st) '(1 node))
    (check-eq? (current-amb-info) 'caller)
    (check-equal? (stream->list st) '((1 node) (2 node)))
    (check-eq? (current-amb-info) 'caller)))

(test-case "custom payload can be a mutable choice descriptor"
  (define payload (box 'candidate-info))
  (check-equal?
   (stream->list
    (in-amb
     (let ([pending '(a b)])
       (make-amb
        (λ () (pair? pending))
        (λ ()
          (define value (car pending))
          (set! pending (cdr pending))
          (list value (unbox (current-amb-info))))
        payload))))
   '((a candidate-info) (b candidate-info))))

(test-case "failed child must not leak its metadata"
  (check-equal?
   (stream->list
    (in-amb
     (amb
      (make-amb (λ () #f) (λ () 'unreachable) 'failed)
      (tagged-choice 'live '(42)))))
   '((42 live))))
