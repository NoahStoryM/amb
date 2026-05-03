#lang racket/base

(require "../main.rkt"
         racket/contract/combinator
         racket/set
         racket/stream
         racket/treelist
         rackunit)

(let ()
  (define (thk)
    (define res '())
    (stream-empty?
     (in-amb
      (let ([x (amb 1 2)])
        (set! res (cons (list x) res))
        (let ([y (amb 3 4)])
          (set! res (cons (list x y) res))
          (let ([z (amb 5 6)])
            (set! res (cons (list x y z) res))
            (amb))))))
    (reverse res))
  (test-case "Test DFS"
    (check-equal?
     (thk)
     '((1) (1 3) (1 3 5)
                 (1 3 6)
           (1 4) (1 4 5)
                 (1 4 6)
       (2) (2 3) (2 3 5)
                 (2 3 6)
           (2 4) (2 4 5)
                 (2 4 6)))
    (check-equal?
     (for/list ([x (in-amb (amb 1 (amb 2 3) (amb 4 5 6) 7 8))]) x)
     '(1 2 3 4 5 6 7 8)))
  (test-case "Test BFS"
    (check-equal?
     (parameterize ([current-amb-depth-first? #f])
       (thk))
     '((1) (2)
       (1 3) (1 4)
       (2 3) (2 4)
       (1 3 5) (1 3 6)
       (1 4 5) (1 4 6)
       (2 3 5) (2 3 6)
       (2 4 5) (2 4 6)))
    (check-equal?
     (parameterize ([current-amb-depth-first? #f])
       (for/list ([x (in-amb (amb 1 (amb 2 3) (amb 4 5 6) 7 8))]) x))
     '(1 7 8 2 3 4 5 6))))

(test-case "Test fair search"
  (check-equal?
   (for/list ([i (in-amb (amb (amb 1 2 3) (amb 'a 'b 'c)))]) i)
   '(1 2 3 a b c))
  (check-equal?
   (parameterize ([current-amb-fair? #t])
     (for/list ([i (in-amb (amb (amb 1 2 3) (amb 'a 'b 'c)))]) i))
   '(1 a 2 b 3 c))
  (define (thk)
    (define a '(0))
    (define b (append a (list (amb 1 2 3))))
    (parameterize ([current-amb-fair? #t])
      (define c (append b (list (amb 4 5 6))))
      c))
  (check-equal?
   (for/list ([i (in-amb* thk)]) i)
   '((0 1 4) (0 2 4) (0 3 4)
     (0 1 5) (0 2 5) (0 3 5)
     (0 1 6) (0 2 6) (0 3 6))))

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
  (check-equal? (for/set ([i (in-stream s)]) i) st)
  (let ([n 0])
    (for/list ([i (in-amb (begin0 (amb 1 2 3) (set! n (add1 n))))] [_ 2]) i)
    (check-eqv? n 3))
  (let ([n 0])
    (for/list ([_ 2] [i (in-amb (begin0 (amb 1 2 3) (set! n (add1 n))))]) i)
    (check-eqv? n 2)))

(test-case "Test multi values in amb"
  (define (thk)
    (let-values ([(x y) (amb (values 2 9) (values 9 2))])
      (when (> x y) (amb))
      (list x y)))
  (check-equal? (stream-first (in-amb* thk)) '(2 9)))

(test-case "Test multi values in for/amb"
  (define (thk)
    (let-values ([(x y)
                  (for/amb #:length 2 ([v '([2 9] [9 2])])
                    (apply values v))])
      (when (> x y) (amb))
      (list x y)))
  (check-equal? (stream-first (in-amb* thk)) '(2 9)))

(test-case "Test nested amb expressions"
  (define (thk)
    (let ([a (amb 'x 'y (let ([b (amb 1 2 3)]) (- b)) 'z)])
      (when (symbol? a) (amb))
      a))
  (check-eq? (stream-first (in-amb* thk)) -1))

(test-case "Test sequence->amb"
  (define sequence->amb (compose make-amb sequence-generate))
  (test-case "Test sequence->amb with list input"
    ;; list path uses sequence-generate internally
    (check-equal?
     (for/list ([x (in-amb (sequence->amb '(1 2 3)))]) x)
     '(1 2 3))
    (check-equal?
     (for/list ([x (in-amb (sequence->amb '(a b c d e)))]) x)
     '(a b c d e))
    ;; single-element list
    (check-equal?
     (for/list ([x (in-amb (sequence->amb '(only)))]) x)
     '(only)))

  (test-case "Test sequence->amb with treelist input"
    ;; treelist path also uses sequence-generate
    (check-equal?
     (for/list ([x (in-amb (sequence->amb (treelist 1 2 3)))]) x)
     '(1 2 3))
    (check-equal?
     (for/list ([x (in-amb (sequence->amb (treelist)))]) x)
     '()))

  (test-case "Test sequence->amb with stream input"
    ;; stream path uses stream-generate
    (check-equal?
     (for/list ([x (in-amb (sequence->amb (in-range 1 4)))]) x)
     '(1 2 3))
    (check-equal?
     (for/list ([x (in-amb (sequence->amb (in-range 0)))]) x)
     '())
    ;; lazy stream
    (check-equal?
     (for/list ([x (in-amb (sequence->amb (for/stream ([i '(a b c)]) i)))]) x)
     '(a b c)))

  (test-case "Test sequence->amb consistency across input types"
    ;; list and stream with equivalent data produce identical results
    (define from-list
      (for/list ([x (in-amb (sequence->amb '(10 20 30 40 50)))]) x))
    (define from-stream
      (for/list ([x (in-amb (sequence->amb (in-range 10 51 10)))]) x))
    (define from-treelist
      (for/list ([x (in-amb (sequence->amb (treelist 10 20 30 40 50)))]) x))
    (check-equal? from-list from-stream)
    (check-equal? from-list from-treelist)
    ;; nested: sequence->amb inside amb returns individual elements
    (define (nested)
      (let ([a (amb 1 2)]
            [b (amb (sequence->amb '(3 4))
                    (sequence->amb (in-range 5 7)))])
        (list a b)))
    (check-equal?
     (for/set ([i (in-amb* nested)]) i)
     (set '(1 4) '(1 3) '(1 6) '(1 5) '(2 6) '(2 5) '(2 4) '(2 3)))))


(test-case "Test #:break / #:final"
  (test-case "#:break with #:length"
    ;; #:break stops generating alternatives after i >= 3.
    (check-equal?
     (for/list ([x (in-amb (for/amb #:length 10 ([i (in-range 10)])
                             #:break (>= i 3) i))])
       x)
     '(0 1 2)))

  (test-case "#:break without #:length"
    ;; Same semantics via the stream path (no #:length).
    (check-equal?
     (for/list ([x (in-amb (for/amb ([i (in-range 10)])
                             #:break (>= i 3) i))])
       x)
     '(0 1 2)))

  (test-case "#:final with #:length"
    ;; #:final includes the iteration where the guard first holds,
    ;; then stops.  i=3 is included, i>=4 are not.
    (check-equal?
     (for/list ([x (in-amb (for/amb #:length 10 ([i (in-range 10)])
                             #:final (>= i 3) i))])
       x)
     '(0 1 2 3)))

  (test-case "#:final without #:length"
    (check-equal?
     (for/list ([x (in-amb (for/amb ([i (in-range 10)])
                             #:final (>= i 3) i))])
       x)
     '(0 1 2 3)))

  (test-case "multiple #:break / #:final"
    ;; Nearest guard wins: #:final at i=3 fires before #:break at i=10.
    (check-equal?
     (for/list ([x (in-amb (for/amb #:length 30 ([i (in-range 30)])
                             #:break (>= i 10)
                             #:final (= i 3)
                             i))])
       x)
     '(0 1 2 3))))

(test-case "Test efficiency"
  (time
   (define m 10000000)
   (define s (in-amb (for/amb #:length m ([i (in-range m)]) i)))
   (void))
  (time
   (define m 10000000)
   (define s (in-amb/do (for/amb #:length m ([i (in-range m)]) i)))
   (void))
  (time
   (define m 10000000)
   (define s (in-amb (for/amb ([i (in-range m)]) i)))
   (void))
  (time
   (define m 10000000)
   (define s (in-amb/do (for/amb ([i (in-range m)]) i)))
   (void))
  (time
   (define m 1000000)
   (define (thk)
     (define n (for/amb #:length (add1 m) ([i (in-inclusive-range 0 m)]) i))
     (when (< n m) (amb))
     n)
   (for/first ([n (in-amb* thk)]) (check-eq? n m)))
  (time
   (define m 1000000)
   (define (thk)
     (define n (for/amb ([i (in-inclusive-range 0 m)]) i))
     (when (< n m) (amb))
     n)
   (for/first ([n (in-amb* thk)]) (check-eq? n m)))
  (time
   (define m 1000000)
   (define (thk)
     (define n (let next ([i 0]) (amb i (next (add1 i)))))
     (when (< n m) (amb))
     n)
   (for/first ([n (in-amb* thk)]) (check-eq? n m)))
  (time
   (for ([i 1000000]
         [(j k) (in-amb (for/amb #:length 1000000 ([i 1000000]) (values i (- i))))])
     (list i j k)))
  (time
   (for ([i 1000000]
         [(j k) (in-amb (for/amb #:length 1000000 #:fill (values 0 0) ([i 1000000]) (values i (- i))))])
     (list i j k)))
  (time
   (for ([i 1000000]
         [(j k) (in-amb (for/amb ([i 1000000]) (values i (- i))))])
     (list i j k)))
  (let ()
    (define (next i j) (amb (values i j) (next (add1 i) (sub1 j))))
    (time
     (for ([i 1000000]
           [(j k) (in-amb (next 0 0))])
       (list i j k))))
  (let ()
    (define (next i j) (amb (values i j) (next (add1 i) (sub1 j))))
    (define s (in-amb (next 0 0)))
    (time
     (for ([i 1000000]
           [(j k) (in-stream s)])
       (list i j k))))
  (let ()
    (define (next j) (amb j (next (add1 j))))
    (time
     (for ([i 1000000]
           [j (in-amb (next 0))])
       (list i j))))
  (let ()
    (define (next j) (amb j (next (add1 j))))
    (define s (in-amb (next 0)))
    (time
     (for ([i 1000000]
           [j (in-stream s)])
       (list i j))))
  (let ([s (in-amb (for/amb #:length 1000000 ([i 1000000]) i))])
    (time (for ([i (in-stream s)]) i)))
  (let ([s (in-amb (for/amb ([i 1000000]) i))])
    (time (for ([i (in-stream s)]) i)))
  (time (for ([i (in-amb (for/amb #:length 1000000 ([i 1000000]) i))]) i))
  (time (for ([i (in-amb (for/amb ([i 1000000]) i))]) i))
  (let ([s (in-amb (for/amb #:length 1000000 ([i 1000000]) i))])
    (time (for ([i (in-stream s)]) i)))
  (let ([s (in-amb (for/amb ([i 1000000]) i))])
    (time (for ([i (in-stream s)]) i)))
  (time (for ([i (in-amb (for/amb #:length 1000000 ([i 1000000]) i))]) i))
  (time (for ([i (in-amb (for/amb ([i 1000000]) i))]) i)))
