#lang racket/base

;; RESEARCH (ambKanren issue #14): observable pause.
;; Run with: raco test pkgs/amb/tests/amb-pause.rkt
(require rackunit racket/sequence racket/stream amb)

(define (P v) (if (eq? v amb-paused) 'P v))
(define (observe thk) (for/list ([v (in-amb*/pause thk)]) (P v)))

(test-case "pause is invisible to in-amb and in-amb/do"
  (check-equal? (for/list ([v (in-amb/do (begin (pause) 1))]) v) '(1))
  (check-equal? (stream->list (in-amb (begin (pause) (amb 1 (begin (pause) 2)))))
                '(1 2)))

(test-case "in-amb/pause reports every pause, in order, and loses nothing"
  (check-equal? (observe (λ () (pause) (amb 1 (begin (pause) 2))))
                '(P 1 P 2))
  (check-equal? (for/list ([v (in-amb/pause (begin (pause) (pause) 'x))]) (P v))
                '(P P x)))

(test-case "a pause only reaches the innermost search"
  ;; the inner in-amb/do does not observe pauses: they only reschedule
  (check-equal?
   (observe (λ ()
              (define-values (more? get)
                (sequence-generate (in-amb/do (begin (pause) 'inner))))
              (pause)
              (list (and (more?) (get)))))
   '(P (inner))))

(test-case "failure is not a pause"
  (check-equal? (observe (λ () (amb (amb) 1 (begin (pause) (amb)) 2)))
                '(1 P 2)))

(test-case "pause under fair / breadth-first strategies keeps the answer set"
  (for* ([df '(#t #f)] [fair '(#t #f)])
    (define vs
      (parameterize ([current-amb-depth-first? df] [current-amb-fair? fair])
        (for/list ([v (in-amb/do (let ([x (amb 1 2 3)]) (pause) x))]) v)))
    (check-equal? (sort vs <) '(1 2 3) (format "df=~a fair=~a" df fair))))

(test-case "a computation that pauses forever yields control each time"
  (define (spin) (pause) (spin))
  (define-values (more? get) (sequence-generate (in-amb*/pause spin)))
  (check-equal? (for/list ([_ 5]) (and (more?) (P (get)))) '(P P P P P)))
