#lang typed/racket/base

(require typed/amb typed/rackunit)

(define delayed : (Sequenceof Integer)
  (parameterize ([current-amb-search-strategy 'hierarchical])
    (in-amb
     (let ([x (amb-local (amb (amb 1 2) 3))])
       (amb-suspend)
       x))))

(check-equal? (for/list : (Listof Integer) ([x delayed]) x) '(1 2 3))

(check-equal?
 (parameterize ([current-amb-search-strategy 'hierarchical])
   (for/list : (Listof Integer)
             ([x (in-amb
                  (for/amb : Integer ([i (in-range 3)])
                    (amb-local (amb i (- i)))))])
     x))
 '(0 0 1 -1 2 -2))

(check-equal?
 (parameterize ([current-amb-search-strategy 'hierarchical])
   (for/list : (Listof (List Integer Integer))
             ([(x y) (in-amb
                      (amb-local
                       (begin
                         (amb-suspend)
                         (amb (values 1 2) (values 3 4)))))])
     (list x y)))
 '((1 2) (3 4)))

(check-equal?
 (parameterize ([current-amb-search-strategy 'hierarchical])
   (for/list : (Listof (List Integer Integer))
             ([(x y) (in-amb
                      (amb-local #:limit 0 #:default (values 3 4)
                                 (error 'amb-local "unused source")))])
     (list x y)))
 '((3 4)))

(check-equal?
 (parameterize ([current-amb-search-strategy 'hierarchical])
   (for/list : (Listof Boolean)
             ([x (in-amb
                  (call-with-values
                   (λ () (amb-local (begin (amb-suspend) (values))))
                   (λ () #t)))])
     x))
 '(#t))

(check-equal?
 (parameterize ([current-amb-search-strategy 'hierarchical])
   (for/list : (Listof Integer)
             ([x (in-amb (amb-local #:limit 1 (amb 1 2)))])
     x))
 '(1))

(check-equal?
 (parameterize ([current-amb-search-strategy 'hierarchical])
   (for/list : (Listof Integer)
             ([x (in-amb
                  (let ([answer (amb-local #:default #f (amb 1 2))])
                    (if answer (+ answer 10) 99)))])
     x))
 '(11 12))
