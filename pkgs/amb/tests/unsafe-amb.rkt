#lang racket/base

;; ===================================================================
;; unsafe-amb* correctness verification suite (Racket 8.10)
;;
;; Stream semantics notes:
;;   in-amb leaves are value lists, but make-stream uses (apply values v*)
;;   to restore multiple values — single-value computation stream elements are bare values.
;;   Multi-value/zero-value leaves need for ([(id ...) (in-amb/do ...)]) to consume.
;;
;; Coverage paths:
;;   - around #xFF (255) threshold
;;   - initial binary compaction (live < 3/4·len)
;;   - no-compaction + tail fail skip (live ≥ 3/4·len)
;;   - re-compaction slice branch (compacted? = #t)
;;   - re-compaction right-to-left scan branch (uncompacted + shuffler shuffle + consumed >half)
;;   - fail (#t) vs unsafe-fail (#f) protocol semantics
;;   - enumeration completeness across four strategy combinations
;; ===================================================================

(require racket/stream
         amb
         (rename-in (only-in amb/private/amb unsafe-amb* thunks fail)
                    [fail amb-fail]))

(define-syntax-rule (enum expr)          ; single-value computation → bare value list
  (stream->list (in-amb expr)))

(define (enum* thk) (stream->list (in-amb* thk)))

(define passes 0)
(define failures '())
(define (assert-equal name actual expected)
  (if (equal? actual expected)
      (begin (set! passes (add1 passes))
             (printf "✓ ~a\n" name))
      (begin (set! failures (cons name failures))
             (printf "✗ ~a\n    actual: ~v\n    expected: ~v\n" name actual expected))))
(define-syntax-rule (test name body)
  (with-handlers ([(λ (e) #t)
                   (λ (e) (set! failures (cons name failures))
                     (printf "✗ ~a [exception]\n    ~a\n" name
                             (if (exn? e) (exn-message e) e)))])
    body))

;; -------------------------------------------------------------------
;; 1. Baseline: small vector (< 256), default DFS, strict left-to-right
;; -------------------------------------------------------------------
(test "P0"
      (assert-equal "P0: small vector order (len=5, DFS)"
                    (enum (amb 1 2 3 4 5)) '(1 2 3 4 5)))

(test "P0b"
      (assert-equal "P0b: nested amb order (DFS)"
                    (enum (list (amb 'a 'b) (amb 1 2)))
                    '((a 1) (a 2) (b 1) (b 2))))

;; -------------------------------------------------------------------
;; 2. fail (#t) semantics: after (amb) fails, remaining parent tree branches must survive
;;    —— this is the key reason fail returns #t not #f
;; -------------------------------------------------------------------
(test "P1"
      (assert-equal "P1: (amb (amb) 5) → (5)"
                    (enum (amb (amb) 5)) '(5)))

(test "P1b"
      (assert-equal "P1b: (amb (amb) (amb) 7 (amb)) → (7)"
                    (enum (amb (amb) (amb) 7 (amb))) '(7)))

(test "P1c"
      (assert-equal "P1c: deep failure doesn't affect sibling branches"
                    (enum (amb (car (list (amb) 1)) 2)) '(2)))

;; -------------------------------------------------------------------
;; 3. Special leaves: #f value, multiple values, zero values
;; -------------------------------------------------------------------
(test "P2"
      (assert-equal "P2: #f as a valid leaf value"
                    (enum (amb #f 'a)) '(#f a)))

(test "P2b"
      (assert-equal "P2b: multiple-value leaf"
                    (for/list ([(a b) (in-amb/do (values (amb 1 2) 10))])
                      (list a b))
                    '((1 10) (2 10))))

(test "P2c"
      (assert-equal "P2c: zero-value leaf (values)"
                    (for/list ([() (in-amb/do (begin (amb 1 2) (values)))])
                      'seen)
                    '(seen seen)))

;; -------------------------------------------------------------------
;; 4. Initial binary compaction (len > 255, live < 3/4·len)
;; -------------------------------------------------------------------
(test "P3"
      (assert-equal "P3: binary compaction (1000 slots / 300 live)"
                    (enum* (λ () (for/amb #:length 1000 ([i 300]) i)))
                    (for/list ([i 300]) i)))

(test "P3b"
      (assert-equal "P3b: binary compaction (256 slots / 100 live)"
                    (enum* (λ () (for/amb #:length 256 ([i 100]) i)))
                    (for/list ([i 100]) i)))

;; -------------------------------------------------------------------
;; 5. No-compaction path (live ≥ 3/4·len, tail fails skipped by get + final unsafe-fail)
;; -------------------------------------------------------------------
(test "P4"
      (assert-equal "P4: no-compaction + tail fail skip (1000 slots / 800 live)"
                    (enum* (λ () (for/amb #:length 1000 ([i 800]) i)))
                    (for/list ([i 800]) i)))

(test "P4b"
      (assert-equal "P4b: exact 3/4 boundary (1000/750, 4·750 not< 3·1000 → no compaction)"
                    (enum* (λ () (for/amb #:length 1000 ([i 750]) i)))
                    (for/list ([i 750]) i)))

;; -------------------------------------------------------------------
;; 6. Re-compaction slice branch (compacted?=#t, consumed >half, triggers multiple times)
;; -------------------------------------------------------------------
(test "P5"
      (assert-equal "P5: re-compaction slice (1000 live all consumed)"
                    (enum* (λ () (for/amb #:length 1000 ([i 1000]) i)))
                    (for/list ([i 1000]) i)))

(test "P5b"
      (assert-equal "P5b: 4097 live all consumed (slice triggers multiple times)"
                    (enum* (λ () (for/amb #:length 4097 ([i 4097]) i)))
                    (for/list ([i 4097]) i)))

;; -------------------------------------------------------------------
;; 7. Re-compaction scan branch
;;    Trigger: len>255 ∧ live≥3/4·len (uncompacted) ∧ shuffler shuffle ∧ consumed >half
;; -------------------------------------------------------------------
(define (vector-reverse! v)
  (define n (vector-length v))
  (for ([i (quotient n 2)])
    (define j (- n 1 i))
    (define t (vector-ref v i))
    (vector-set! v i (vector-ref v j))
    (vector-set! v j t)))

(test "P6"
      (assert-equal "P6: scan branch (400 slots / 320 live + reverse shuffler)"
                    (parameterize ([current-amb-shuffler vector-reverse!])
                      (enum* (λ () (for/amb #:length 400 ([i 320]) i))))
                    (for/list ([i 320]) (- 319 i))))

(define ((fisher-yates! seed) v)
  (define prng (make-pseudo-random-generator))
  (parameterize ([current-pseudo-random-generator prng])
    (random-seed seed)
    (define n (vector-length v))
    (for ([i (in-range (sub1 n) 0 -1)])
      (define j (random (add1 i)))
      (define t (vector-ref v i))
      (vector-set! v i (vector-ref v j))
      (vector-set! v j t))))

(test "P6b"
      (let ([r (parameterize ([current-amb-shuffler (fisher-yates! 42)])
                 (enum* (λ () (for/amb #:length 1024 ([i 800]) i))))])
        (assert-equal "P6b: scan branch (Fisher-Yates, set completeness)"
                      (list (length r) (sort r <))
                      (list 800 (for/list ([i 800]) i)))))

(test "P6c"
      (let ([r (parameterize ([current-amb-shuffler (fisher-yates! 7)])
                 (enum* (λ () (for/amb #:length 100 ([i 60]) i))))])
        (assert-equal "P6c: shuffler + small vector (len<256)"
                      (sort r <) (for/list ([i 60]) i))))

;; -------------------------------------------------------------------
;; 8. All fail / empty vector: slot-0 check → (fail)=#t → parent tree continues
;; -------------------------------------------------------------------
(test "P7"
      (assert-equal "P7: all-fill vector as sibling branch → parent tree survives"
                    (enum (amb (for/amb #:length 300 ([x '()]) x) 42))
                    '(42)))

(test "P7b"
      (assert-equal "P7b: empty vector direct call to unsafe-amb*"
                    (enum (amb (unsafe-amb* (vector)) 9))
                    '(9)))

(test "P7c"
      (assert-equal "P7c: #:length 0 → immediate fail"
                    (enum (amb (for/amb #:length 0 () 1) 8))
                    '(8)))

;; -------------------------------------------------------------------
;; 9. Four strategy combinations: completeness + exact DFS order
;; -------------------------------------------------------------------
(define (strategy-enum df? fair?)
  (parameterize ([current-amb-depth-first? df?]
                 [current-amb-fair? fair?])
    (stream->list (in-amb (list (amb 'a 'b 'c) (amb 1 2))))))

(test "P8"
      (assert-equal "P8: DFS exact order"
                    (strategy-enum #t #f)
                    '((a 1) (a 2) (b 1) (b 2) (c 1) (c 2))))

(define (sort-any xs)
  (sort xs (λ (x y) (string<? (format "~a" x) (format "~a" y)))))

(test "P8b"
      (let ([expected (sort-any (strategy-enum #t #f))])
        (assert-equal "P8b: four strategies produce same set"
                      (for*/list ([df? '(#t #f)] [fair? '(#t #f)])
                        (sort-any (strategy-enum df? fair?)))
                      (list expected expected expected expected))))

(printf "\nStrategy order comparison ((amb a b c) × (amb 1 2)):\n")
(for* ([df? '(#t #f)] [fair? '(#t #f)])
  (printf "  depth-first?=~a fair?=~a → ~a\n" df? fair?
          (strategy-enum df? fair?)))

;; Deeper three-level tree, to make depth-first? differences visible
(define (strategy-enum3 df? fair?)
  (parameterize ([current-amb-depth-first? df?]
                 [current-amb-fair? fair?])
    (stream->list (in-amb (list (amb 'a 'b) (amb 1 2) (amb 'x 'y))))))

(printf "\nThree-level tree strategy order comparison:\n")
(for* ([df? '(#t #f)] [fair? '(#t #f)])
  (printf "  depth-first?=~a fair?=~a → ~a\n" df? fair?
          (strategy-enum3 df? fair?)))

(test "P8c"
      (let ([expected (sort-any (strategy-enum3 #t #f))])
        (assert-equal "P8c: three-level tree four-strategy set consistency (8 leaves)"
                      (for*/list ([df? '(#t #f)] [fair? '(#t #f)])
                        (sort-any (strategy-enum3 df? fair?)))
                      (list expected expected expected expected))))

(test "P8d"
      (let ([expected (for/list ([i 1000]) i)])
        (assert-equal "P8d: large scale (1000 alt) four-strategy set consistency"
                      (for*/list ([df? '(#t #f)] [fair? '(#t #f)])
                        (sort (parameterize ([current-amb-depth-first? df?]
                                             [current-amb-fair? fair?])
                                (enum* (λ () (for/amb #:length 1000 ([i 1000]) i))))
                              <))
                      (list expected expected expected expected))))

;; -------------------------------------------------------------------
;; 10. Parameterize mid-tree strategy switch (per-tree parameter capture)
;; -------------------------------------------------------------------
(test "P9"
      (let ([r (stream->list
                (in-amb
                 (list (amb 'x 'y)
                       (parameterize ([current-amb-depth-first? #f]
                                      [current-amb-fair? #t])
                         (amb 1 2 3)))))])
        (assert-equal "P9: subtree strategy switch doesn't lose solutions"
                      (list (length r) (sort (map cadr r) <))
                      (list 6 '(1 1 2 2 3 3)))))

;; -------------------------------------------------------------------
;; 11. Laziness: stream only drives to needed depth
;; -------------------------------------------------------------------
(test "P10"
      (let ()
        (define side-effects '())
        (define s (in-amb* (λ ()
                             (define v (amb 1 2 3 4 5))
                             (set! side-effects (cons v side-effects))
                             v)))
        (void (stream-first s))
        (void (stream-first (stream-rest s)))
        (assert-equal "P10: taking first 2 leaves only executes first 2 branches"
                      (reverse side-effects) '(1 2))))

;; -------------------------------------------------------------------
;; 12. for/amb without #:length (stream path) + body nested amb
;; -------------------------------------------------------------------
(test "P11"
      (assert-equal "P11: for/amb stream path basic enumeration"
                    (enum* (λ () (for/amb ([x '(10 20 30)]) x)))
                    '(10 20 30)))

(test "P11b"
      (let ([r (enum* (λ () (for/amb ([x '(1 2)]) (amb x (- x)))))])
        (assert-equal "P11b: for/amb stream path + body nested amb"
                      (sort r <) '(-2 -1 1 2))))

(test "P11c"
      (assert-equal "P11c: for/amb #:break"
                    (enum* (λ () (for/amb ([x '(1 2 3 4 5)]) #:break (> x 3) x)))
                    '(1 2 3)))

(test "P11d"
      (assert-equal "P11d: for/amb infinite sequence laziness (take first 5)"
                    (for/list ([x (in-amb/do (for/amb ([n (in-naturals)]) n))]
                               [_ 5])
                      x)
                    '(0 1 2 3 4)))

(test "P11e"
      (let ([r (for/list ([x (in-amb/do
                              (for/amb ([n (in-naturals)]) (amb n (- n))))]
                          [_ 6])
                 x)])
        (assert-equal "P11e: infinite sequence + body nested amb (take first 6)"
                      (sort r <) '(-2 -1 0 0 1 2))))

;; -------------------------------------------------------------------
;; 13. in-amb/do (do-sequence path) consistent with stream path
;; -------------------------------------------------------------------
(test "P12"
      (assert-equal "P12: in-amb/do consistent with in-amb"
                    (for/list ([x (in-amb/do (amb 1 2 3))]) x)
                    '(1 2 3)))

(test "P12b"
      (assert-equal "P12b: in-amb/do nested"
                    (for/list ([x (in-amb/do (+ (amb 10 20) (amb 1 2)))]) x)
                    '(11 12 21 22)))

;; -------------------------------------------------------------------
(printf "\n===== ~a passed / ~a failed =====\n" passes (length failures))
(unless (null? failures)
  (printf "Failed: ~a\n" (reverse failures))
  (exit 1))
