#lang typed/racket/base/no-check

;; The examples of The Reasoned Schemer, first edition, on trs1.rkt
;; (chapter 9 looks at the streams of the book, and is left out).

(require (except-in racket/match ==)
         (for-syntax racket/base racket/path)
         "trs1.rkt"
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

(book-examples "trs1"
               "ch1.rktl" "ch2.rktl" "ch3.rktl" "ch4.rktl" "ch5.rktl" "ch6.rktl"
               "ch7.rktl" "ch8.rktl" "ch10.rktl")

(check-book "TRS1" '((same . 228)))
