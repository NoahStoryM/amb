#lang racket/base

;; The supply of a vector choice, before and after its slots are
;; cleared as they are taken.
;;
;;   before   the shuffler, then (sequence->amb thk*): the generators of
;;            `sequence-generate*`, which keep the whole vector
;;   after    `choose` of private/choice.rkt: the shuffler, then
;;            `mutable-vector-generate*`
;;
;; Both run in the same process, in alternating rounds, so that the
;; order does not favor either.  This only records times; the numbers
;; vary from run to run by more than the difference that they show.
;;
;;   raco make pkgs/amb/tests/bench-choice.rkt
;;   racket pkgs/amb/tests/bench-choice.rkt [iterations] [rounds]
;;
;; Running the module itself does nothing (it is under tests/, and
;; `raco test` runs it); the benchmark is its `main` submodule.

(module+ main
  (require racket/list
           racket/vector
           amb
           (prefix-in choice: "../private/choice.rkt"))

  (define (before thk*)
    (with-amb-mark ((current-amb-shuffler) thk*))
    (sequence->amb thk*))
  (define after choice:choose)

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

  (define cases
    (list (cons "2 alternatives" (flat 2))
          (cons "8 alternatives" (flat 8))
          (cons "32 alternatives" (flat 32))
          (cons "nested 4×4×4, ÷10" (nested))))

  (define (run-ms thk n)
    (collect-garbage)
    (define-values (_ cpu real gc) (time-apply (λ () (for ([_ (in-range n)]) (thk))) '()))
    cpu)

  (define (median xs) (list-ref (sort xs <) (quotient (length xs) 2)))

  (printf "Racket ~a (~a), ~a iterations, ~a rounds; CPU ms: median [min, max]\n"
          (version) (system-type 'vm) iterations rounds)
  (for ([c (in-list cases)])
    (define name (car c))
    (define n (if (regexp-match? #rx"÷10" name) (quotient iterations 10) iterations))
    (define b ((cdr c) before))
    (define a ((cdr c) after))
    (unless (= (b) (a)) (error 'bench-choice "different answers: ~a" name))
    (b) (a)                                ; warm up
    (define-values (bs as)
      (for/fold ([bs '()] [as '()]) ([r (in-range rounds)])
        (if (even? r)
            (let* ([tb (run-ms b n)] [ta (run-ms a n)]) (values (cons tb bs) (cons ta as)))
            (let* ([ta (run-ms a n)] [tb (run-ms b n)]) (values (cons tb bs) (cons ta as))))))
    (define (show xs) (format "~a [~a, ~a]" (median xs) (apply min xs) (apply max xs)))
    (printf "~a~a  before ~a   after ~a\n"
            name (make-string (max 0 (- 20 (string-length name))) #\space)
            (show bs) (show as))))
