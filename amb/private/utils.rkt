#lang racket/base

(require racket/treelist
         racket/mutable-treelist
         racket/vector)
(provide (all-defined-out))


(define (vector-reverse! v)
  (define len (vector-length v))
  (when (>= len 2)
    (for ([i (in-range 0 len)]
          [j (in-range (sub1 len) -1 -1)])
      #:final (<= (- j i) 2)
      (define vi (vector-ref v i))
      (define vj (vector-ref v j))
      (vector-set! v i vj)
      (vector-set! v j vi))))

(define (mutable-treelist-pop! mtl)
  (begin0 (mutable-treelist-last mtl)
    (mutable-treelist-delete! mtl (sub1 (mutable-treelist-length mtl)))))


(struct exn:fail:contract:amb exn:fail:contract ()
  #:extra-constructor-name make-exn:fail:contract:amb
  #:transparent)

(define (raise-amb-error)
  (raise (exn:fail:contract:amb
          "amb: empty amb tasks;\n expected at least one amb task\n  in: (amb)"
          (current-continuation-marks))))

(define current-amb-empty-handler (make-parameter raise-amb-error))
(define current-amb-shuffler (make-parameter vector-reverse!))
(define current-amb-tasks    (make-parameter (mutable-treelist)))
(define current-amb-pusher   (make-parameter mutable-treelist-add!))
(define current-amb-popper   (make-parameter mutable-treelist-pop!))

(define (schedule-amb-tasks! k alt* [tasks (current-amb-tasks)])
  (define shuffle! (current-amb-shuffler))
  (if (= 0 (vector-length alt*))
      (shuffle! alt*)
      (let ([push! (current-amb-pusher)])
        (define (alt->amb-task alt)
          (define (amb-task) (call-in-continuation k alt))
          amb-task)
        (cond
          [(or (eq? push! mutable-treelist-add!)
               (eq? push! mutable-treelist-cons!))
           (cond
             [(eq? push! mutable-treelist-add!) (shuffle! alt*)]
             [(eq? shuffle! vector-reverse!) (void)]
             #;[(eq? shuffle! vector-shuffle!) (shuffle! alt*)]
             [else (shuffle! alt*) (vector-reverse! alt*)])
           (vector-map! alt->amb-task alt*)
           (define new-tasks (vector->treelist alt*))
           (if (eq? push! mutable-treelist-add!)
               (mutable-treelist-append!  tasks new-tasks)
               (mutable-treelist-prepend! new-tasks tasks))]
          [else
           (shuffle! alt*)
           (for ([alt (in-vector alt*)])
             (push! tasks (alt->amb-task alt)))]))))
