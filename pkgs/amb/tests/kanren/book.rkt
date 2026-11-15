#lang racket/base

;; The examples of The Reasoned Schemer, as they are transcribed in the
;; tests of the package miniKanren-typed.  That package is not a
;; dependency of this one: when it is not installed, there are no
;; examples, and the modules that run them say so.

(require (for-syntax racket/base)
         (prefix-in ru: rackunit)
         "record.rkt")

(provide book-examples check-book)

;; (book-examples edition file ...) is the forms of the files, which are
;; in the collection typed/miniKanren/<edition>/tests; or nothing.
;; Every (check-equal? actual expected) of the forms is a record of
;; record.rkt, through the `check-equal?` of the module that uses this.
(define-syntax (book-examples stx)
  (syntax-case stx ()
    [(_ edition file ...)
     (let ()
       (define (path-of file)
         (collection-file-path file "typed" "miniKanren" (syntax-e #'edition) "tests"
                               #:fail (λ (message) #f)))
       ;; the forms, in the lexical context of the use of this macro
       (define (in-context form)
         (cond
           [(syntax? form)
            (datum->syntax stx (in-context (syntax-e form)) form form)]
           [(pair? form) (cons (in-context (car form)) (in-context (cdr form)))]
           [else form]))
       (define (forms-of path)
         (call-with-input-file path
           (λ (in)
             (port-count-lines! in)
             (let loop ()
               (define form (read-syntax path in))
               (if (eof-object? form)
                   '()
                   (cons (in-context form) (loop)))))))
       (define paths (map path-of (syntax->datum #'(file ...))))
       (if (andmap values paths)
           #`(begin (available! #t) #,@(apply append (map forms-of paths)))
           #'(available! #f)))]))

(define available? #f)
(define (available! ?) (set! available? ?))

;; Checks the number of examples of each class, after they have run.
(define (check-book name expected)
  (cond
    [available?
     (finish! name #f)
     (ru:check-equal? (summary) expected (format "~a: ~s" name (not-same)))]
    [else
     (printf "~a: skipped (the package miniKanren-typed is not installed)\n" name)]))
