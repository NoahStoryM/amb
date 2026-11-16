#lang racket/base

(require "../base.rkt"
         "sequence.rkt"
         (for-syntax racket/base syntax/parse)
         racket/stream)

(provide for/stream for*/stream
         empty-stream
         raise-empty-stream-error
         stream-generate
         make-stream
         stream-flatten)


(define (raise-empty-stream-error)
  (raise (exn:fail:contract
          "stream has no more values"
          (current-continuation-marks))))


(define (stream-generate s)
  (values
   (λ () (not (stream-empty? s)))
   (λ ()
     (if (stream-empty? s)
         (raise-empty-stream-error)
         (begin0 (stream-first s)
           (set! s (stream-rest s)))))))

;; The stream of the elements that `next` generates, in the shape of
;; `sequence-generate*`: (next) returns (values (or/c list? #f) next).
;; The stream remembers its elements: `next`, and each procedure that
;; it returns, is called until it has returned once, and not again.
;;
;; A stream of `stream-cons` keeps its elements in delays, and a delay
;; whose expression raises is broken for good.  Here a cell keeps the
;; procedure until it has returned: when (next) raises, the cell is as
;; it was, and the next read of the stream calls the same procedure
;; again.  What that means is up to the procedure.
;;
;;   state   the procedure that has yet to be called; or
;;           (cons vals rest), an element and the stream after it; or
;;           #t, the end
(struct cached ([state #:mutable])
  #:property prop:custom-write
  (λ (s port mode) (write-string "#<stream>" port))
  #:methods gen:stream
  [(define (stream-empty? s)
     (eq? (force! s) #t))
   (define (stream-first s)
     (let ([state (force! s)])
       (if (pair? state)
           (apply values (car state))
           (raise-empty-stream-error))))
   (define (stream-rest s)
     (let ([state (force! s)])
       (if (pair? state)
           (cdr state)
           (raise-empty-stream-error))))])

(define (force! s)
  (let ([state (cached-state s)])
    (if (procedure? state)
        (let-values ([(v* next) (state)])
          (let ([state (if v* (cons v* (cached next)) #t)])
            (set-cached-state! s state)
            state))
        state)))

(define (make-stream next)
  (cached next))


(define (stream-flatten* s*)
  (stream*
   (if (stream-empty? s*)
       s*
       (let*-values ([(v . v*) (stream-first s*)])
         (define s (stream-rest s*))
         (if (and (null? v*) (stream? v))
             (if (stream-empty? v)
                 (stream-flatten* s)
                 (stream-flatten* (stream* (stream-first v) (stream-rest v) s)))
             (stream-cons (apply values v v*) (stream-flatten* s)))))))

(define (stream-flatten/do s*)
  (define (next)
    (let loop ([s* s*])
      (if (stream-empty? s*)
          (values #f raise-empty-stream-error)
          (let*-values ([(v . v*) (stream-first s*)])
            (define s (stream-rest s*))
            (if (and (null? v*) (stream? v))
                (if (stream-empty? v)
                    (loop s)
                    (loop (stream* (stream-first v) (stream-rest v) s)))
                (values (cons v v*) (λ () (loop s))))))))
  (define-values (more? get) (generate*->generate next))
  (make-do-sequence
   (λ ()
     (initiate-sequence
      #:init-pos           0
      #:continue-with-pos? more?
      #:pos->element       get
      #:next-pos           add1))))

(define-sequence-syntax stream-flatten
  (λ () #'stream-flatten*)
  (λ (stx)
    (syntax-parse stx
      [[(id:id ...) (_ expr)]
       (syntax/loc stx
         [(id ...)
          (stream-flatten/do expr)])])))
