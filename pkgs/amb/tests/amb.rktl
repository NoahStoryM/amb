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
  (check-eq? (in-amb* amb*) empty-stream)
  (check-eq? (in-amb*/do amb*) empty-sequence)
  (define s (in-amb* thk))
  (check-equal? (for/set ([i (in-stream s)]) i) st)
  (check-equal? (for/set ([i (in-stream s)]) i) st)
  (let ([n 0])
    (for/list ([i (in-amb (begin0 (amb 1 2 3) (set! n (add1 n))))] [_ 2]) i)
    (check-eqv? n 3))
  (let ([n 0])
    (for/list ([_ 2] [i (in-amb (begin0 (amb 1 2 3) (set! n (add1 n))))]) i)
    (check-eqv? n 2))
  (let ([p (make-parameter 0)])
    (define s
      (parameterize ([p 1])
        (in-amb (amb 0 (p) 2))))
    (check-equal?
     (stream->list (parameterize ([p 3]) s))
     '(0 1 2)))
  (let ([n 0] [t '()])
    (define s
      (dynamic-wind
        (λ ()
          (set! t (cons (cons n 'pre) t)))
        (λ ()
          (in-amb
           (begin
             (amb 0 1)
             (set! t (cons (cons n 'values) t))
             'data)))
        (λ ()
          (set! t (cons (cons n 'past) t))
          (set! n (add1 n)))))
    (stream-first (stream-rest s))
    (check-equal?
     t
     '([2 . past] [2 . values] [2 . pre]
       [1 . past] [1 . values] [1 . pre]
       [0 . past] [0 . pre]))))

(test-case "Test multi values in amb"
  (define (thk)
    (parameterize ([current-amb-tasks ((current-amb-maker))])
      (let-values ([(x y) (amb (values 2 9) (values 9 2))])
        (when (> x y) (amb))
        (list x y))))
  (check-equal? (thk) '(2 9)))

(test-case "Test multi values in for/amb"
  (define (thk)
    (parameterize ([current-amb-tasks ((current-amb-maker))])
      (let-values ([(x y)
                    (for/amb #:length 2 ([v '([2 9] [9 2])])
                      (apply values v))])
        (when (> x y) (amb))
        (list x y))))
  (check-equal? (thk) '(2 9)))

(test-case "Test nested amb expressions"
  (define (thk)
    (parameterize ([current-amb-tasks ((current-amb-maker))])
      (let ([a (amb 'x 'y (let ([b (amb 1 2 3)]) (- b)) 'z)])
        (when (symbol? a) (amb))
        a)))
  (check-eq? (thk) -1))

(test-case "Test efficiency"
  (parameterize ([current-amb-tasks ((current-amb-maker))])
    (time
     (define m 10000000)
     (define n (for/amb #:length m ([i (in-range m)]) i))
     (void)))
  (parameterize ([current-amb-tasks ((current-amb-maker))])
    (time
     (define m 10000000)
     (define n (for/amb ([i (in-range m)]) i))
     (void)))
  (parameterize ([current-amb-tasks ((current-amb-maker))])
    (time
     (define m 1000000)
     (define n (for/amb #:length (add1 m) ([i (in-inclusive-range 0 m)]) i))
     (when (< n m) (amb))
     (check-eq? n m)))
  (parameterize ([current-amb-tasks ((current-amb-maker))])
    (time
     (define m 1000000)
     (define n (for/amb ([i (in-inclusive-range 0 m)]) i))
     (when (< n m) (amb))
     (check-eq? n m)))
  (parameterize ([current-amb-tasks ((current-amb-maker))])
    (time
     (define m 1000000)
     (define n (let next ([i 0]) (amb i (next (add1 i)))))
     (when (< n m) (amb))
     (check-eq? n m)))
  (parameterize ([current-amb-tasks ((current-amb-maker))])
    (time
     (for ([i 1000000]
           [(j k) (in-amb* (λ () (for/amb #:length 1000000 ([i 1000000]) (values i (- i)))))])
       (list i j k))))
  (parameterize ([current-amb-tasks ((current-amb-maker))])
    (time
     (for ([i 1000000]
           [(j k) (in-amb* (λ () (for/amb ([i 1000000]) (values i (- i)))))])
       (list i j k))))
  (parameterize ([current-amb-tasks ((current-amb-maker))])
    (define (next i j) (amb (values i j) (next (add1 i) (sub1 j))))
    (time
     (for ([i 1000000]
           [(j k) (in-amb* (λ () (next 0 0)))])
       (list i j k))))
  (parameterize ([current-amb-tasks ((current-amb-maker))])
    (define (next i j) (amb (values i j) (next (add1 i) (sub1 j))))
    (define s (in-amb* (λ () (next 0 0))))
    (time
     (for ([i 1000000]
           [(j k) (in-stream s)])
       (list i j k))))
  (parameterize ([current-amb-tasks ((current-amb-maker))])
    (define (next j) (amb j (next (add1 j))))
    (time
     (for ([i 1000000]
           [j (in-amb (next 0))])
       (list i j))))
  (parameterize ([current-amb-tasks ((current-amb-maker))])
    (define (next j) (amb j (next (add1 j))))
    (define s (in-amb (next 0)))
    (time
     (for ([i 1000000]
           [j (in-stream s)])
       (list i j))))
  (parameterize ([current-amb-shuffler void])
    (define s (in-amb (for/amb #:length 1000000 ([i 1000000]) i)))
    (time (for ([i (in-stream s)]) i)))
  (parameterize ([current-amb-shuffler void])
    (define s (in-amb (for/amb ([i 1000000]) i)))
    (time (for ([i (in-stream s)]) i)))
  (parameterize ([current-amb-shuffler void])
    (time (for ([i (in-amb (for/amb #:length 1000000 ([i 1000000]) i))]) i)))
  (parameterize ([current-amb-shuffler void])
    (time (for ([i (in-amb (for/amb ([i 1000000]) i))]) i)))
  (let ()
    (define s (in-amb (for/amb #:length 1000000 ([i 1000000]) i)))
    (time (for ([i (in-stream s)]) i)))
  (let ()
    (define s (in-amb (for/amb ([i 1000000]) i)))
    (time (for ([i (in-stream s)]) i)))
  (time (for ([i (in-amb (for/amb #:length 1000000 ([i 1000000]) i))]) i))
  (time (for ([i (in-amb (for/amb ([i 1000000]) i))]) i)))
