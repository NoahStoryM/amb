#lang racket/base

;; Run explicitly with a clean baseline worktree, not during raco test:
;; racket hierarchical-bench.rkt /path/to/amb-baseline
;; This measures enumeration overhead, not miniKanren performance.
(module+ main
  (require racket/list racket/runtime-path "../private/hierarchical.rkt")
  (define-runtime-path current-core "../private/amb.rkt")
  (define args (current-command-line-arguments))
  (unless (= (vector-length args) 1)
    (error 'hierarchical-bench "expected baseline amb worktree path"))
  (define baseline-core
    (build-path (vector-ref args 0) "pkgs" "amb" "private" "amb.rkt"))
  (define (load-engine path)
    (list (dynamic-require path 'make-amb)
          (dynamic-require path 'in-amb*/do)))
  (define baseline (load-engine baseline-core))
  (define current (load-engine current-core))
  (define (work engine hierarchical?)
    (define choose (car engine))
    (define (range n)
      (define i 0)
      (choose (lambda () (< i n))
              (lambda () (begin0 i (set! i (add1 i))))))
    (define (body) (+ (* 1000 (range 200)) (range 1000)))
    (define results
      (if hierarchical?
          (search->sequence (search/thunk body))
          ((cadr engine) body)))
    (for/sum ([x results]) x))
  (printf "Racket ~a; 200,000 answers; one warmup and seven measured runs\n"
          (version))
  (for ([entry (list (list 'baseline baseline #f)
                    (list 'flat current #f)
                    (list 'hierarchical current #t))])
    (define (run) (work (cadr entry) (caddr entry)))
    (run)
    (define timings
      (for/list ([_ (in-range 7)])
        (collect-garbage)
        (define-values (answers cpu real gc) (time-apply run '()))
        (unless (equal? answers '(19999900000))
          (error 'hierarchical-bench "incorrect sum: ~a" answers))
        (list cpu real gc)))
    (printf "~a: median cpu/real/gc ms ~a; samples ~a\n"
            (car entry)
            (for/list ([i (in-range 3)])
              (list-ref (sort (map (lambda (v) (list-ref v i)) timings) <) 3))
            timings)))
