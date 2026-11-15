;; The examples of The Reasoned Schemer, second edition, chapters 1 to
;; 9, on trs2.rkt.  The module that includes this file sets the merger.

(require (except-in racket/match ==)
         (only-in racket/list make-list)
         (for-syntax racket/base racket/path)
         "trs2.rkt"
         "book.rkt"
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

(book-examples "trs2"
               "ch1.rktl" "ch2.rktl" "ch3.rktl" "ch4.rktl" "ch5.rktl" "ch6.rktl"
               "arith.rktl" "ch7.rktl" "ch8.rktl" "ch9.rktl")
