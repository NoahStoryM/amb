#lang racket/base

;; First-class frontiers: `make-amb-frontier`, `amb-frontier-length`,
;; `in-amb #:frontier`.
(require rackunit racket/stream racket/list racket/sequence amb)

(test-case "make-amb-frontier: a fresh, empty frontier with a handler"
  (define (h) 'H)
  (define f (make-amb-frontier h))
  (check-true (amb-frontier? f))
  (check-false (amb-frontier? 'f))
  (check-eq? (amb-frontier-empty-handler f) h)
  (check-equal? (amb-frontier-length f) 0)
  ;; by default the handler of the current search, or amb-fail outside
  (check-eq? (amb-frontier-empty-handler (make-amb-frontier)) amb-fail)
  (check-equal? (stream->list
                 (in-amb (eq? (amb-frontier-empty-handler (make-amb-frontier)) h)
                         #:empty-handler h))
                '(#t))
  ;; an inner search without a handler has the default one
  (check-equal? (stream->list
                 (in-amb (stream->list
                          (in-amb (eq? (amb-frontier-empty-handler (make-amb-frontier)) amb-fail)))
                         #:empty-handler h))
                '((#t))))

(test-case "amb-frontier-length: the trees waiting; the running one is not counted"
  (define f (make-amb-frontier))
  (define (len) (amb-frontier-length f))
  (check-exn exn:fail:contract? (λ () (amb-frontier-length 'f)))
  ;; the root tree is put back after each branch
  (check-equal? (stream->list (in-amb (len) #:frontier f)) '(0))
  (check-equal? (stream->list (in-amb (amb (len) (len) (len)) #:frontier f)) '(1 1 1))
  (check-equal? (stream->list (in-amb (list (amb 'a 'b) (len)) #:frontier f)) '((a 1) (b 1)))
  (check-equal? (len) 0))

(test-case "#:frontier: the search runs a given frontier, with its handler"
  (define f (make-amb-frontier (λ () 'F)))
  (check-equal? (stream->list (in-amb (amb 1 (amb) 2) #:frontier f)) '(1 F 2))
  (check-equal? (for/list ([x (in-amb (amb 1 (amb) 2) #:frontier f)]) x) '(1 F 2))
  (check-equal? (for/list ([x (in-amb/do (amb 1 (amb) 2) #:frontier f)]) x) '(1 F 2))
  (check-equal? (for/list ([x (in-amb* (λ () (amb 1 (amb) 2)) #:frontier f)]) x) '(1 F 2))
  (check-equal? (stream->list (in-amb* (λ () (amb 1 (amb) 2)) #:frontier f)) '(1 F 2))
  (check-equal? (for/list ([x (in-amb*/do (λ () (amb 1 (amb) 2)) #:frontier f)]) x) '(1 F 2))
  (check-equal? (amb-frontier-length f) 0)
  ;; not both
  (check-exn #rx"cannot both"
             (λ () (stream->list (in-amb* (λ () 1) (λ () 'h) #:frontier f))))
  (check-exn exn:fail:contract? (λ () (in-amb* (λ () 1) #:frontier 'f))))

(test-case "a handler can avoid the recursion with a search on another frontier"
  (define inner (make-amb-frontier (λ () 'inner)))
  (define (handler) (stream->list (in-amb (list 'h (amb)) #:frontier inner)))
  (check-equal? (stream->list (in-amb (amb 1 (amb) 2) #:empty-handler handler))
                '(1 (inner) 2)))

(test-case "a frontier shared by two searches: their trees take turns in it"
  (define f (make-amb-frontier))
  ;; the second search, started inside the first, runs the same items:
  ;; here what is left of the first one's root (the tree being resumed
  ;; is not in the frontier while it runs, so 'b comes after)
  (check-equal?
   (stream->list
    (in-amb (amb 'a
                 (stream->list (in-amb (amb 1 2) #:frontier f))
                 'b)
            #:frontier f))
   '(a (1 2) b))
  ;; the choice being resumed is not in the frontier, so the second
  ;; search does not resume it
  (define log '())
  (define g (make-amb-frontier))
  (check-equal?
   (stream->list
    (in-amb (let ([x (amb 'x 'y)])
              (set! log (cons x log))
              (if (eq? x 'x)
                  (length (stream->list (in-amb (amb 1 2) #:frontier g)))
                  'done))
            #:frontier g))
   '(2 done))
  (check-equal? (reverse log) '(x y)))

(test-case "an escape in a search nested on the same frontier: the outer one loses nothing"
  (define f (make-amb-frontier))
  (define first? #t)
  (define-values (more? get)
    (sequence-generate
     (in-amb/do (let ([x (amb 'z 'a 'b 'c)])
                  (when (and first? (eq? x 'a))
                    (set! first? #f)
                    (for ([y (in-amb/do (amb 'i0 (error "boom") 'i1 'i2) #:frontier f)]) (void)))
                  x)
                #:frontier f)))
  (define (step) (with-handlers ([exn:fail? exn-message]) (if (more?) (get) 'end)))
  ;; the inner search's trees were left in the shared frontier, so the
  ;; outer one runs them too
  (check-equal? (for/list ([_ 7]) (step)) '(z "boom" b c i1 i2 end)))

(test-case "an escape in the first step does not push the root again"
  (define f (make-amb-frontier))
  (define n 0)
  (define-values (more? get)
    (sequence-generate
     (in-amb/do (begin (set! n (add1 n)) (when (= n 1) (error "first")) (amb 1 2))
                #:frontier f)))
  (define (step) (with-handlers ([exn:fail? exn-message]) (if (more?) (get) 'end)))
  ;; the computation raised, which uses it up, as for any alternative
  (check-equal? (for/list ([_ 2]) (step)) '("first" end))
  (check-equal? n 1)
  (check-equal? (amb-frontier-length f) 0)
  ;; the same with a frontier of its own
  (set! n 0)
  (define-values (more2? get2)
    (sequence-generate
     (in-amb/do (begin (set! n (add1 n)) (when (= n 1) (error "first")) (amb 1 2)))))
  (check-equal? (for/list ([_ 2]) (with-handlers ([exn:fail? exn-message]) (if (more2?) (get2) 'end)))
                '("first" end))
  (check-equal? n 1))

(test-case "an abandoned search leaves its trees in a given frontier"
  (define f (make-amb-frontier))
  (check-equal? (for/first ([x (in-amb/do (amb 1 2 3) #:frontier f)]) x) 1)
  (check-equal? (for/list ([x (in-amb/do (amb 'p 'q) #:frontier f)]) x) '(p q 2 3)))

;; ---- Local frontiers ----

(define (pause) (when (amb #t #f) (amb)))

(test-case "a local frontier inherits the empty handler"
  (check-equal? (stream->list
                 (in-amb (amb/local 1 (amb) 2) #:empty-handler (λ () 'R)))
                '(1 R 2)))

(test-case "running out is a dead end, never an (amb)"
  ;; the handler is called only for the explicit (amb)
  (define calls 0)
  (check-equal? (stream->list
                 (in-amb (amb/local (amb/local 1 2) (amb/local) 3)
                         #:empty-handler (λ () (set! calls (add1 calls)) 'P)))
                '(1 2 P 3))
  (check-equal? calls 1))

(test-case "the turn ends where the handler returns"
  ;; a handler that fails does not end a turn; the default one does
  (define (s1) (amb/local 1 2 3 (amb) 4 5 6))
  (define (s2) (amb/local 7 8 (amb) 9 (amb)))
  (check-equal? (stream->list (in-amb (amb/local (s1) (s2)))) '(1 2 3 7 8 4 5 6 9))
  (check-equal? (stream->list (in-amb (amb/local (s1) (s2)) #:empty-handler (λ () (amb-fail))))
                '(1 2 3 4 5 6 7 8 9)))

(test-case "amb-frontier-length: a mounted local frontier counts as one item"
  (define root (make-amb-frontier))
  (check-equal? (stream->list
                 (in-amb (amb/local (amb 'p (begin (amb 1 2 3)
                                                   (amb-frontier-length root))))
                         #:frontier root))
                '(p 1 1 1)))

(test-case "a choice after an amb/local returns is held in the local frontier"
  ;; its (amb) is answered by the outer handler and ends a local turn
  (check-equal? (stream->list
                 (in-amb (let ([v (amb/local 1 2)]) (list v (amb 'x (amb) 'y)))
                         #:empty-handler (λ () 'P)))
                '((1 x) P (2 x) P (1 y) (2 y)))
  (check-equal? (stream->list
                 (in-amb (let ([v (amb 1 2)]) (list v (amb 'x (amb) 'y)))
                         #:empty-handler (λ () 'P)))
                '((1 x) P (1 y) (2 x) P (2 y)))
  ;; and after the local frontier has run out of its own trees
  (define r
    (stream->list
     (in-amb (amb/local 'x
                        (let ([v (amb/local 'a (begin (pause) 'b))])
                          (list v (amb 1 2))))
             #:empty-handler (λ () 'P))))
  (check-equal? (sort (filter pair? r) string<? #:key (λ (p) (format "~a" p)))
                '((a 1) (a 2) (b 1) (b 2)))
  (check-equal? (count (λ (v) (eq? v 'P)) r) 1)
  (check-equal? (car r) 'x))

(test-case "after the handler raises inside a local frontier the search goes on"
  (define first? #t)
  (define-values (more? get)
    (sequence-generate
     (in-amb/do (amb/local (amb/local 1 (amb) 2) 3)
                #:empty-handler (λ () (if first? (begin (set! first? #f) (error "boom")) 'P)))))
  (check-equal? (get) 1)
  (check-exn #rx"boom" get)
  ;; the raise ended nothing: (amb) was used up, and no turn ended there
  (check-equal? (for/list ([_ 2]) (get)) '(2 3))
  (check-false (more?)))
