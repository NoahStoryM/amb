#lang racket/base

(require racket/set data/queue rackunit)
(require "../main.rkt")


(let ()
  (define (thk)
    (define res '())
    (let ([x (amb 1 2)])
      (set! res (cons (list x) res))
      (let ([y (amb 3 4)])
        (set! res (cons (list x y) res))
        (let ([z (amb 5 6)])
          (set! res (cons (list x y z) res))
          (amb* (reverse res))))))
  (test-case "DFS"
    (parameterize ([current-amb-queue (make-queue)])
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
    (parameterize ([current-amb-queue (make-queue)]
                   [current-amb-shuffler values]
                   [current-amb-enqueue! enqueue!])
      (check-equal?
       (thk)
       '((1) (2)
         (1 3) (1 4)
         (2 3) (2 4)
         (1 3 5) (1 3 6)
         (1 4 5) (1 4 6)
         (2 3 5) (2 3 6)
         (2 4 5) (2 4 6))))))

(test-case "Test in-amb[/thunk]"
  (define (thk)
    (let ([x (amb 7 4 0)]
          [y (amb 3 8 2)])
      (unless (<= x y) (amb))
      (list x y)))
  (define res (list->set '((7 8) (4 8) (0 3) (0 8) (0 2))))
  (check-equal? (for/set ([i (in-amb/thunk thk)]) i) res)
  (check-equal? (for/set ([i (in-amb (thk))]) i) res))

(test-case "Test multi values in amb"
  (define (thk)
    (parameterize ([current-amb-queue (make-queue)])
      (let-values ([(x y) (amb (values 2 9) (values 9 2))])
        (when (> x y) (amb))
        (list x y))))
  (check-equal? (thk) '(2 9)))

(test-case "Test multi values in for/amb"
  (define (thk)
    (parameterize ([current-amb-queue (make-queue)])
      (let-values ([(x y)
                    (for/amb ([v '([2 9] [9 2])])
                      (apply values v))])
        (when (> x y) (amb))
        (list x y))))
  (check-equal? (thk) '(2 9)))

(test-case "Test nested amb expressions"
  (define (thk)
    (parameterize ([current-amb-queue (make-queue)])
      (let ([a (amb 'x 'y (let ([b (amb 1 2 3)]) (- b)) 'z)])
        (when (symbol? a) (amb))
        a)))
  (check-eq? (thk) -1))

(test-case "Test efficiency"
  (parameterize ([current-amb-queue (make-queue)])
    (time (check-eq? 99999 (let ([i (for/amb ([i 100000]) i)]) (amb*) i))))
  (parameterize ([current-amb-queue (make-queue)])
    (time
     (define m 100000)
     (define n (for/amb ([i (in-inclusive-range 0 m)]) i))
     (when (< n m) (amb))
     (check-eq? n m)))
  (parameterize ([current-amb-queue (make-queue)])
    (time
     (define m 100000)
     (define n (let next ([i 0]) (amb i (next (add1 i)))))
     (when (< n m) (amb))
     (check-eq? n m)))
  (parameterize ([current-amb-queue (make-queue)])
    (define (next i j) (amb (values i j) (next (add1 i) (sub1 j))))
    (time
     (for ([i (in-range 100000)]
           [(j k) (in-amb/thunk (λ () (next 0 0)))])
       (list i j k))))
  (parameterize ([current-amb-queue (make-queue)])
    (define (next i j) (amb (values i j) (next (add1 i) (sub1 j))))
    (define s (in-amb/thunk (λ () (next 0 0))))
    (time
     (for ([i (in-range 100000)]
           [(j k) s])
       (list i j k))))
  (parameterize ([current-amb-queue (make-queue)])
    (define (next j) (amb j (next (add1 j))))
    (time
     (for ([i (in-range 100000)]
           [j (in-amb (next 0))])
       (list i j))))
  (parameterize ([current-amb-queue (make-queue)])
    (define (next j) (amb j (next (add1 j))))
    (define s (in-amb (next 0)))
    (time
     (for ([i (in-range 100000)]
           [j s])
       (list i j)))))
