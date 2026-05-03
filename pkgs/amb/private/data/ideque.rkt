#lang racket/base

(require racket/match)
(provide (rename-out [make-ideque ideque])
         list->ideque
         empty-ideque
         ideque-empty?
         ideque?
         ideque-length
         ideque-reverse
         ideque-add-front
         ideque-add-back
         ideque-pop-front
         ideque-pop-back
         ideque-front
         ideque-back
         ideque-remove-front
         ideque-remove-back)


(struct ideque (front-size front-chain back-size back-chain))
(define ideque-length (match-λ [(ideque fs _ bs _) (+ fs bs)]))
(define (make-ideque . ls) (list->ideque ls))
(define (list->ideque ls)
  (if (null? ls)
      empty-ideque
      (ideque (length ls) ls 0 '())))

(define empty-ideque (ideque 0 '() 0 '()))
(define ideque-empty?
  (match-λ
    [(ideque 0 '() 0 '()) #t]
    [(? ideque?) #f]))

(define ideque-reverse
  (match-λ
    [(ideque 0 '() 0 '()) empty-ideque]
    [(ideque bs bc fs fc) (ideque fs fc bs bc)]))

(define (ideque-add-front dq v)
  (match-define (ideque fs fc bs bc) dq)
  (ideque (add1 fs) (cons v fc) bs bc))
(define (ideque-add-back  dq v)
  (match-define (ideque fs fc bs bc) dq)
  (ideque fs fc (add1 bs) (cons v bc)))

(define (ideque-pop-front dq)
  (match dq
    [(ideque 0 '() 0 '())
     (raise-argument-error 'ideque-pop-front "(not/c ideque-empty?)" dq)]
    [(or (ideque 1 (list v) 0 '())
         (ideque 0 '() 1 (list v)))
     (values v empty-ideque)]
    [(ideque fs (cons v fc) bs bc)
     (values v (ideque (sub1 fs) fc bs bc))]
    [(ideque 0 '() fs bc)
     (define fc (reverse bc))
     (values (car fc) (ideque (sub1 fs) (cdr fc) 0 '()))]))
(define (ideque-pop-back dq)
  (match dq
    [(ideque 0 '() 0 '())
     (raise-argument-error 'ideque-pop-back "(not/c ideque-empty?)" dq)]
    [(or (ideque 1 (list v) 0 '())
         (ideque 0 '() 1 (list v)))
     (values v empty-ideque)]
    [(ideque fs fc bs (cons v bc))
     (values v (ideque fs fc (sub1 bs) bc))]
    [(ideque bs fc 0 '())
     (define bc (reverse fc))
     (values (car bc) (ideque 0 '() (sub1 bs) (cdr bc)))]))

(define (ideque-front dq)
  (match dq
    [(ideque 0 '() 0 '())
     (raise-argument-error 'ideque-front "(not/c ideque-empty?)" dq)]
    [(ideque _ (cons v _) _ _) v]
    [(ideque 0 '() _ bc)
     (define fc (reverse bc))
     (car fc)]))
(define (ideque-back dq)
  (match dq
    [(ideque 0 '() 0 '())
     (raise-argument-error 'ideque-back "(not/c ideque-empty?)" dq)]
    [(ideque _ _ _ (cons v _)) v]
    [(ideque _ fc 0 '())
     (define bc (reverse fc))
     (car bc)]))

(define (ideque-remove-front dq)
  (match dq
    [(ideque 0 '() 0 '())
     (raise-argument-error 'ideque-remove-front "(not/c ideque-empty?)" dq)]
    [(ideque fs (cons _ fc) bs bc)
     (ideque (sub1 fs) fc bs bc)]
    [(ideque 0 '() fs bc)
     (define fc (reverse bc))
     (ideque (sub1 fs) (cdr fc) 0 '())]))
(define (ideque-remove-back dq)
  (match dq
    [(ideque 0 '() 0 '())
     (raise-argument-error 'ideque-remove-back "(not/c ideque-empty?)" dq)]
    [(ideque fs fc bs (cons _ bc))
     (ideque fs fc (sub1 bs) bc)]
    [(ideque bs fc 0 '())
     (define bc (reverse fc))
     (ideque 0 '() (sub1 bs) (cdr bc))]))
