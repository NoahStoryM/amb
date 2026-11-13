;; The examples of The Reasoned Schemer, second edition, chapters 1 to 9,
;; as transcribed in the tests of miniKanren-typed.  The module that
;; includes this file provides the language and `record!`; every
;; (check-equal? actual expected) of the examples becomes a record.

(define-syntax (check-equal? stx)
  (syntax-case stx ()
    [(_ actual expected . _)
     (with-syntax ([location
                    (format "~a:~a"
                            (path->string (file-name-from-path (syntax-source stx)))
                            (syntax-line stx))])
       (syntax/loc stx
         (record! location (λ () actual) expected)))]))

(include (lib "typed/miniKanren/trs2/tests/ch1.rktl"))
(include (lib "typed/miniKanren/trs2/tests/ch2.rktl"))
(include (lib "typed/miniKanren/trs2/tests/ch3.rktl"))
(include (lib "typed/miniKanren/trs2/tests/ch4.rktl"))
(include (lib "typed/miniKanren/trs2/tests/ch5.rktl"))
(include (lib "typed/miniKanren/trs2/tests/ch6.rktl"))
(include (lib "typed/miniKanren/trs2/tests/arith.rktl"))
(include (lib "typed/miniKanren/trs2/tests/ch7.rktl"))
(include (lib "typed/miniKanren/trs2/tests/ch8.rktl"))
(include (lib "typed/miniKanren/trs2/tests/ch9.rktl"))
