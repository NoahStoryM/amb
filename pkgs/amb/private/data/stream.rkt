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

(define (make-stream next)
  (stream*
   (let-values ([(v* next) (next)])
     (if v*
         (stream-cons (apply values v*) (make-stream next))
         empty-stream))))


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
