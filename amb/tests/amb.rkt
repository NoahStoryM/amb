#lang racket/base

(require racket/contract/combinator
         racket/mutable-treelist
         racket/set
         racket/stream
         rackunit)
(require "../main.rkt")

(let ()
  (define (thk)
    (let/cc k
      (define res '())
      (define (return) (k (reverse res)))
      (parameterize ([current-amb-empty-handler return])
        (let ([x (amb 1 2)])
          (set! res (cons (list x) res))
          (let ([y (amb 3 4)])
            (set! res (cons (list x y) res))
            (let ([z (amb 5 6)])
              (set! res (cons (list x y z) res))
              (amb)))))))
  (test-case "DFS"
    (parameterize ([current-amb-tasks (mutable-treelist)])
      (check-equal?
       (thk)
       '((1) (1 3) (1 3 5)
                   (1 3 6)
             (1 4) (1 4 5)
                   (1 4 6)
         (2) (2 3) (2 3 5)
                   (2 3 6)
             (2 4) (2 4 5)
                   (2 4 6)))))
  (test-case "BFS"
    (parameterize ([current-amb-tasks    (mutable-treelist)]
                   [current-amb-shuffler void]
                   [current-amb-pusher   mutable-treelist-cons!])
      (check-equal?
       (thk)
       '((1) (2)
         (1 3) (1 4)
         (2 3) (2 4)
         (1 3 5) (1 3 6)
         (1 4 5) (1 4 6)
         (2 3 5) (2 3 6)
         (2 4 5) (2 4 6))))))

(test-case "Test in-amb[*]"
  (define (thk)
    (let ([x (amb 7 4 0)]
          [y (amb 3 8 2)])
      (unless (<= x y) (amb))
      (list x y)))
  (define ls '((7 8) (4 8) (0 3) (0 8) (0 2)))
  (define st (list->set ls))
  (check-equal? (for/set ([i (in-amb* thk)])  i) st)
  (check-equal? (for/set ([i (in-amb (thk))]) i) st)
  (check-exn exn:fail:contract:blame? (λ () (in-amb* 123)))
  (check-exn exn:fail:contract:blame? (λ () (for ([i (in-amb* 123)] [j '()]) i)))
  (define s (in-amb* thk))
  (check-equal? (for/set ([i (in-stream s)]) i) st)
  (check-equal? (for/set ([i (in-stream s)]) i) st))

(test-case "Test multi values in amb"
  (define (thk)
    (parameterize ([current-amb-tasks (mutable-treelist)])
      (let-values ([(x y) (amb (values 2 9) (values 9 2))])
        (when (> x y) (amb))
        (list x y))))
  (check-equal? (thk) '(2 9)))

(test-case "Test multi values in for/amb"
  (define (thk)
    (parameterize ([current-amb-tasks (mutable-treelist)])
      (let-values ([(x y)
                    (for/amb #:length 2 ([v '([2 9] [9 2])])
                      (apply values v))])
        (when (> x y) (amb))
        (list x y))))
  (check-equal? (thk) '(2 9)))

(test-case "Test nested amb expressions"
  (define (thk)
    (parameterize ([current-amb-tasks (mutable-treelist)])
      (let ([a (amb 'x 'y (let ([b (amb 1 2 3)]) (- b)) 'z)])
        (when (symbol? a) (amb))
        a)))
  (check-eq? (thk) -1))

(test-case "Test efficiency"
  (parameterize ([current-amb-tasks (mutable-treelist)])
    (time
     (define m 10000000)
     (define n (for/amb #:length m ([i (in-range m)]) i))
     (void)))
  (parameterize ([current-amb-tasks (mutable-treelist)])
    (time
     (define m 1000000)
     (define n (for/amb #:length (add1 m) ([i (in-inclusive-range 0 m)]) i))
     (when (< n m) (amb))
     (check-eq? n m)))
  (parameterize ([current-amb-tasks (mutable-treelist)])
    (time
     (define m 1000000)
     (define n (let next ([i 0]) (amb i (next (add1 i)))))
     (when (< n m) (amb))
     (check-eq? n m)))
  (parameterize ([current-amb-tasks (mutable-treelist)])
    (time
     (for ([i 1000000]
           [(j k) (in-amb* (λ () (for/amb #:length 1000000 ([i 1000000]) (values i (- i)))))])
       (list i j k))))
  (parameterize ([current-amb-tasks (mutable-treelist)])
    (define (next i j) (amb (values i j) (next (add1 i) (sub1 j))))
    (time
     (for ([i 1000000]
           [(j k) (in-amb* (λ () (next 0 0)))])
       (list i j k))))
  (parameterize ([current-amb-tasks (mutable-treelist)])
    (define (next i j) (amb (values i j) (next (add1 i) (sub1 j))))
    (define s (in-amb* (λ () (next 0 0))))
    (time
     (for ([i 1000000]
           [(j k) (in-stream s)])
       (list i j k))))
  (parameterize ([current-amb-tasks (mutable-treelist)])
    (define (next j) (amb j (next (add1 j))))
    (time
     (for ([i 1000000]
           [j (in-amb (next 0))])
       (list i j))))
  (parameterize ([current-amb-tasks (mutable-treelist)])
    (define (next j) (amb j (next (add1 j))))
    (define s (in-amb (next 0)))
    (time
     (for ([i 1000000]
           [j (in-stream s)])
       (list i j))))
  (parameterize ([current-amb-shuffler void])
    (define s (in-amb (for/amb #:length 1000000([i 1000000]) i)))
    (time (for ([i (in-stream s)]) i)))
  (parameterize ([current-amb-shuffler void])
    (time (for ([i (in-amb (for/amb #:length 1000000([i 1000000]) i))]) i)))
  (let ()
    (define s (in-amb (for/amb #:length 1000000([i 1000000]) i)))
    (time (for ([i (in-stream s)]) i)))
  (time (for ([i (in-amb (for/amb #:length 1000000([i 1000000]) i))]) i)))
