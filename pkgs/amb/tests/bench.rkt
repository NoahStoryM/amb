#lang racket/base

;; ===================================================================
;; amb performance benchmark v2: strategy comparison + hand-written baseline
;; Timing: warmup 1x → 3 rounds, repeat reps times per round, report median per iteration
;; ===================================================================

(require racket/stream amb)

(define (median xs) (list-ref (sort xs <) (quotient (length xs) 2)))

(define last-result #f)

(define (bench! name thk #:reps [reps 1])
  (set! last-result (thk))                ; warmup + record result
  (define per-run
    (for/list ([_ 3])
      (collect-garbage) (collect-garbage)
      (define-values (v* cpu real gc)
        (time-apply (λ () (for ([_ reps]) (thk))) '()))
      (cons (/ cpu reps 1.0) (/ gc reps 1.0))))
  (define cpu (median (map car per-run)))
  (define gc  (median (map cdr per-run)))
  (printf "~a~a: ~a ms (gc ~a) → ~v\n"
          name
          (make-string (max 0 (- 40 (string-utf-8-length name))) #\space)
          (/ (round (* cpu 1000)) 1000.0)
          (/ (round (* gc 1000)) 1000.0)
          last-result)
  cpu)

(define (string-utf-8-length s)           ; approximate CJK alignment
  (for/sum ([c s]) (if (> (char->integer c) 127) 2 1)))

(define (bench/s! name df? fair? thk #:reps [reps 1])
  (bench! name #:reps reps
          (λ () (parameterize ([current-amb-depth-first? df?]
                               [current-amb-fair? fair?])
                  (thk)))))

(define (watchdog! name thk #:timeout [timeout-ms 3000])
  (define result #f)
  (define t0 (current-inexact-milliseconds))
  (define worker (thread (λ () (set! result (thk)))))
  (define done? (sync/timeout (/ timeout-ms 1000.0) worker))
  (unless done? (kill-thread worker))
  (define dt (- (current-inexact-milliseconds) t0))
  (printf "~a~a: ~a\n"
          name (make-string (max 0 (- 40 (string-utf-8-length name))) #\space)
          (if done?
              (format "~a ms → ~v" (/ (round (* dt 100)) 100.0) result)
              (format "diverged (>~a ms timeout)" timeout-ms)))
  (and done? dt))

;; ===================================================================
;; 1. Width pressure: single node N branches full enumeration, report amortized cost per branch
;; ===================================================================
(printf "===== 1. Width pressure (N=100000, full enumeration sum) =====\n")
(define N 100000)

(define t-raw
  (bench! "raw for/in-range (lower bound)" #:reps 50
          (λ () (for/sum ([i N]) i))))

(define t-wide-do
  (bench! "amb·in-amb/do + #:length" #:reps 3
          (λ () (for/sum ([x (in-amb/do (for/amb #:length N ([i N]) i))]) x))))

(bench! "amb·in-amb stream + #:length" #:reps 3
        (λ () (for/sum ([x (in-amb (for/amb #:length N ([i N]) i))]) x)))

(bench! "amb·in-amb/do + stream path (no #:length)" #:reps 3
        (λ () (for/sum ([x (in-amb/do (for/amb ([i N]) i))]) x)))

(printf "→ amortized per branch ≈ ~a µs (one tree protocol round-trip)\n"
        (/ (round (* (/ (* t-wide-do 1000) N) 1000)) 1000.0))

;; ===================================================================
;; 2. Depth pressure: nested single-branch amb, per-layer shift capture grows with context
;;    If total cost is O(D²), doubling D should ≈ 4× time
;; ===================================================================
(printf "\n===== 2. Depth pressure (nested capture scalability, take first leaf) =====\n")

(define (deep-amb d)
  (let loop ([d d])
    (if (zero? d) 0 (add1 (amb (loop (sub1 d)))))))

(define deep-times
  (for/list ([d '(1000 2000 4000 8000 16000)])
    (cons d (bench! (format "depth D=~a" d)
                    #:reps (max 1 (quotient 4000 d))
                    (λ () (stream-first (in-amb (deep-amb d))))))))

(printf "→ doubling ratio: ~a\n"
        (for/list ([a deep-times] [b (cdr deep-times)])
          (if (zero? (cdr a)) '? (/ (round (* 100 (/ (cdr b) (cdr a)))) 100.0))))

;; ===================================================================
;; 3. N-Queens: amb four strategies vs hand-written
;; ===================================================================
(define (safe? col placed)
  (for/and ([c (in-list placed)] [d (in-naturals 1)])
    (and (not (= col c))
         (not (= (abs (- col c)) d)))))

(define (queens/amb n)
  (let loop ([row 0] [placed '()])
    (if (= row n)
        placed
        (let ([col (for/amb #:length n ([c n]) c)])
          (unless (safe? col placed) (amb))
          (loop (add1 row) (cons col placed))))))

(define (count-queens/amb n)
  (for/sum ([_ (in-amb/do (queens/amb n))]) 1))

(define (count-queens/rec n)
  (let loop ([row 0] [placed '()])
    (if (= row n)
        1
        (for/sum ([col n])
          (if (safe? col placed)
              (loop (add1 row) (cons col placed))
              0)))))

(define (count-queens/stack n)
  (let loop ([stack (list (cons 0 '()))] [count 0])
    (if (null? stack)
        count
        (let* ([node (car stack)] [stack (cdr stack)]
                                  [row (car node)] [placed (cdr node)])
          (if (= row n)
              (loop stack (add1 count))
              (loop (for/fold ([s stack]) ([col n])
                      (if (safe? col placed)
                          (cons (cons (add1 row) (cons col placed)) s)
                          s))
                    count))))))

(define (count-queens/queue n)
  (let loop ([front (list (cons 0 '()))] [back '()] [count 0])
    (cond
      [(and (null? front) (null? back)) count]
      [(null? front) (loop (reverse back) '() count)]
      [else
       (let* ([node (car front)]
              [row (car node)] [placed (cdr node)])
         (if (= row n)
             (loop (cdr front) back (add1 count))
             (loop (cdr front)
                   (for/fold ([b back]) ([col n])
                     (if (safe? col placed)
                         (cons (cons (add1 row) (cons col placed)) b)
                         b))
                   count)))])))

(printf "\n===== 3a. 10-Queens · all solutions (724) =====\n")
(define nq 10)
(define t-man10
  (bench! "hand-written · recursive DFS"    #:reps 3 (λ () (count-queens/rec nq))))
(bench! "hand-written · explicit stack DFS"   #:reps 3 (λ () (count-queens/stack nq)))
(bench! "hand-written · explicit queue BFS" #:reps 3 (λ () (count-queens/queue nq)))
(define t-amb10
  (bench/s! "amb · DFS (default)" #t #f (λ () (count-queens/amb nq))))
(bench/s! "amb · DFS+fair"     #t #t (λ () (count-queens/amb nq)))
(bench/s! "amb · BFS"          #f #f (λ () (count-queens/amb nq)))
(bench/s! "amb · BFS+fair"     #f #t (λ () (count-queens/amb nq)))
(printf "→ amb-DFS / hand-written recursive ≈ ~a×\n"
        (/ (round (* 10 (/ t-amb10 (max t-man10 0.001)))) 10.0))

(printf "\n===== 3b. 11-Queens · all solutions (2680, DFS only) =====\n")
(define t-man11
  (bench! "hand-written · recursive DFS"  (λ () (count-queens/rec 11))))
(define t-amb11
  (bench/s! "amb · DFS (default)" #t #f (λ () (count-queens/amb 11))))
(printf "→ amb-DFS / hand-written recursive ≈ ~a×\n"
        (/ (round (* 10 (/ t-amb11 (max t-man11 0.001)))) 10.0))

;; ===================================================================
;; 4. 10-Queens · first-solution latency
;; ===================================================================
(printf "\n===== 4. 10-Queens · first-solution latency =====\n")

(define (first-queens/rec n)
  (let/ec return
    (let loop ([row 0] [placed '()])
      (if (= row n)
          (return placed)
          (for ([col n])
            (when (safe? col placed)
              (loop (add1 row) (cons col placed))))))
    #f))

(bench! "hand-written · recursive DFS first sol" #:reps 30 (λ () (first-queens/rec nq)))
(bench/s! "amb · DFS first sol"      #t #f #:reps 10
          (λ () (stream-first (in-amb (queens/amb nq)))))
(bench/s! "amb · DFS+fair first sol" #t #t
          (λ () (stream-first (in-amb (queens/amb nq)))))
(bench/s! "amb · BFS first sol"      #f #f
          (λ () (stream-first (in-amb (queens/amb nq)))))
(bench/s! "amb · BFS+fair first sol" #f #t
          (λ () (stream-first (in-amb (queens/amb nq)))))

;; ===================================================================
;; 5. Infinite domain: Pythagorean triples — strategy termination + sustained fair-search throughput
;; ===================================================================
(printf "\n===== 5a. Infinite domain · first triple (termination) =====\n")

(define (pyth-triple)
  (define a (for/amb ([n (in-naturals 1)]) n))
  (define b (for/amb ([n (in-naturals 1)]) n))
  (define c (for/amb ([n (in-naturals 1)]) n))
  (unless (= (+ (* a a) (* b b)) (* c c)) (amb))
  (list a b c))

(define (first-pyth df? fair?)
  (parameterize ([current-amb-depth-first? df?]
                 [current-amb-fair? fair?])
    (stream-first (in-amb (pyth-triple)))))

(watchdog! "amb · DFS"      (λ () (first-pyth #t #f)))
(watchdog! "amb · DFS+fair" (λ () (first-pyth #t #t)))
(watchdog! "amb · BFS"      (λ () (first-pyth #f #f)))
(watchdog! "amb · BFS+fair" (λ () (first-pyth #f #t)))

(printf "\n===== 5b. Infinite domain · first 8 triples (fair-search throughput) =====\n")

(define (pyth-k k df? fair?)
  (parameterize ([current-amb-depth-first? df?]
                 [current-amb-fair? fair?])
    (stream->list (stream-take (in-amb (pyth-triple)) k))))

(define (pyth-k/manual k)
  (let loop ([s 3] [acc '()] [n 0])
    (if (= n k)
        (reverse acc)
        (let ([found (for*/list ([a (in-range 1 s)]
                                 [b (in-range a (- s a))]
                                 #:do [(define c (- s a b))]
                                 #:when (and (>= c b)
                                             (= (+ (* a a) (* b b)) (* c c))))
                       (list a b c))])
          (let take ([fs found] [acc acc] [n n])
            (cond
              [(= n k) (reverse acc)]
              [(null? fs) (loop (add1 s) acc n)]
              [else (take (cdr fs) (cons (car fs) acc) (add1 n))]))))))

(bench! "hand-written · diagonal enumeration first 8" #:reps 10 (λ () (pyth-k/manual 8)))
(watchdog! "amb · DFS+fair first 8" (λ () (pyth-k 8 #t #t)) #:timeout 30000)
(watchdog! "amb · BFS+fair first 8" (λ () (pyth-k 8 #f #t)) #:timeout 30000)

;; ===================================================================
;; 6. Uneven-depth tree: strategy leaf-order semantics demo
;; ===================================================================
(printf "\n===== 6. Uneven-depth tree leaf order (amb (+ 10 (amb 1 2)) 100) =====\n")
(for* ([df? '(#t #f)] [fair? '(#t #f)])
  (printf "  depth-first?=~a fair?=~a → ~a\n" df? fair?
          (parameterize ([current-amb-depth-first? df?]
                         [current-amb-fair? fair?])
            (stream->list (in-amb (amb (+ 10 (amb 1 2)) 100))))))
