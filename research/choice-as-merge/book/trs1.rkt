#lang typed/racket/base/no-check

;; The examples of The Reasoned Schemer, first edition, on ../kanren1.rkt
;; (chapter 9 looks at the streams of the book, and is left out, as in
;; ambKanren).

(require racket/include
         (except-in racket/match ==)
         (for-syntax racket/base racket/path)
         "../kanren1.rkt"
         "record.rkt")

(define-syntax (check-equal? stx)
  (syntax-case stx ()
    [(_ actual expected . _)
     (with-syntax ([location
                    (format "~a:~a"
                            (path->string (file-name-from-path (syntax-source stx)))
                            (syntax-line stx))])
       (syntax/loc stx
         (record! location (λ () actual) expected)))]))

(include (lib "typed/miniKanren/trs1/tests/ch1.rktl"))
(include (lib "typed/miniKanren/trs1/tests/ch2.rktl"))
(include (lib "typed/miniKanren/trs1/tests/ch3.rktl"))
(include (lib "typed/miniKanren/trs1/tests/ch4.rktl"))
(include (lib "typed/miniKanren/trs1/tests/ch5.rktl"))
(include (lib "typed/miniKanren/trs1/tests/ch6.rktl"))
(include (lib "typed/miniKanren/trs1/tests/ch7.rktl"))
(include (lib "typed/miniKanren/trs1/tests/ch8.rktl"))
(include (lib "typed/miniKanren/trs1/tests/ch10.rktl"))

(finish! "trs1" (build-path "results" "trs1.rktd"))
