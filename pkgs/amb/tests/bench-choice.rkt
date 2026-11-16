#lang racket/base

;; The supply of a vector choice, in three versions:
;;
;;   seq       the shuffler, then (sequence->amb thk*): the generators of
;;             `sequence-generate*`, which keep the whole vector (c0c8fd9)
;;   clear     the slots are cleared as they are taken, and the vector is
;;             never copied (b5b05ff; a copy of it is below)
;;   compact   `choose` of private/choice.rkt: the slots are cleared, and
;;             a long vector is compacted once half of it is taken
;;
;; All run in the same process, in rotating order, so that the order
;; does not favor any of them.  This only records times; the numbers
;; vary from run to run, and a difference smaller than that is not a
;; result.
;;
;;   raco make pkgs/amb/tests/bench-choice.rkt
;;   racket pkgs/amb/tests/bench-choice.rkt [iterations] [rounds]
;;
;; Running the module itself does nothing (it is under tests/, and
;; `raco test` runs it); the benchmark is its `main` submodule.

(module+ main
  (require racket/vector
           amb
           (only-in "../private/generator.rkt" end)
           (prefix-in choice: "../private/choice.rkt"))

  (define (seq thk*)
    (with-amb-mark ((current-amb-shuffler) thk*))
    (sequence->amb thk*))

  ;; mutable-vector-generate* of b5b05ff, without compaction
  (define (clear-generate* vec)
    (define len (vector-length vec))
    (define pos 0)
    (define ((take i))
      (unless (eq? i pos) (error 'clear-generate* "called again"))
      (set! pos (add1 i))
      (if (< i len)
          (let ([v (vector-ref vec i)])
            (vector-set! vec i #f)
            (values (list v) (take (add1 i))))
          (end)))
    ((take 0)))
  (define (clear thk*)
    (with-amb-mark ((current-amb-shuffler) thk*))
    (make-amb (λ () (clear-generate* thk*))))

  (define compact choice:choose)

  (define versions (list (cons "seq" seq) (cons "clear" clear) (cons "compact" compact)))

  (define-values (iterations rounds)
    (let ([args (current-command-line-arguments)])
      (values (if (> (vector-length args) 0) (string->number (vector-ref args 0)) 100000)
              (if (> (vector-length args) 1) (string->number (vector-ref args 1)) 7))))

  ;; the number of answers of a search of choices made by `choose`
  (define (count-answers thk)
    (for/fold ([n 0]) ([_ (in-amb* thk)]) (add1 n)))

  ;; n alternatives, all enumerated
  (define ((flat n) choose)
    (define thk* (build-vector n (λ (i) (λ () i))))
    (λ () (count-answers (λ () ((choose (vector-copy thk*)))))))

  ;; three nested choices among 4: 64 answers in one search
  (define ((nested) choose)
    (define (four) ((choose (vector (λ () 0) (λ () 1) (λ () 2) (λ () 3)))))
    (λ () (count-answers (λ () (list (four) (four) (four))))))

  ;; name, case, and the divisor of the iterations
  (define cases
    (list (list "2 alternatives" (flat 2) 1)
          (list "8 alternatives" (flat 8) 1)
          (list "32 alternatives" (flat 32) 1)
          (list "nested 4×4×4" (nested) 10)
          (list "256 alternatives" (flat 256) 10)
          (list "1024 alternatives" (flat 1024) 40)
          (list "16384 alternatives" (flat 16384) 640)))

  (define (run-ms thk n)
    (collect-garbage)
    (define-values (_ cpu real gc) (time-apply (λ () (for ([_ (in-range n)]) (thk))) '()))
    cpu)

  (define (median xs) (list-ref (sort xs <) (quotient (length xs) 2)))
  (define (rotate xs r) (append (list-tail xs r) (for/list ([x xs] [_ r]) x)))

  (printf "Racket ~a (~a), ~a iterations / divisor, ~a rounds; CPU ms: median [min, max]\n"
          (version) (system-type 'vm) iterations rounds)
  (for ([c (in-list cases)])
    (define-values (name make n) (values (car c) (cadr c) (quotient iterations (caddr c))))
    (define thks (for/list ([v (in-list versions)]) (make (cdr v))))
    (define answers (for/list ([thk (in-list thks)]) (thk)))  ; warm up
    (unless (apply = answers) (error 'bench-choice "different answers: ~a" name))
    (define times (make-vector (length thks) '()))
    (for ([r (in-range rounds)])
      (for ([i (in-list (rotate (for/list ([i (length thks)]) i) (modulo r (length thks))))])
        (vector-set! times i (cons (run-ms (list-ref thks i) n) (vector-ref times i)))))
    (printf "~a (×~a)\n" name n)
    (for ([v (in-list versions)] [xs (in-vector times)])
      (printf "  ~a~a ~a [~a, ~a]\n" (car v) (make-string (- 8 (string-length (car v))) #\space)
              (median xs) (apply min xs) (apply max xs)))))
