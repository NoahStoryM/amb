#lang racket/base

;; The type of a merger keeps the type of the answers.
;;
;; `Merger` quantifies over the list of return values with dotted variables.
;; The first part checks, by expanding typed modules, what the type
;; checker accepts and rejects.  The second part records what happens
;; at run time when the merger comes from untyped code: the guarantee
;; of the type is about typed mergers only.

(require rackunit
         racket/port
         racket/runtime-path
         (for-syntax racket/base))

(define-runtime-path here ".")

;; Expands (and so type-checks) a module with the given body; returns
;; the namespace it is declared in.
(define (declare! name lang . body)
  (define ns (make-base-namespace))
  (parameterize ([current-namespace ns]
                 [current-load-relative-directory here]
                 [current-directory here]
                 ;; the type checker also prints its errors
                 [current-error-port (open-output-nowhere)])
    (eval `(module ,name ,lang ,@body)))
  ns)

;; Declares the module and runs it; returns what it printed.
(define (run! name lang . body)
  (define ns (apply declare! name lang body))
  (define out (open-output-string))
  (parameterize ([current-namespace ns]
                 [current-output-port out])
    (dynamic-require `(quote ,name) #f))
  (get-output-string out))

(define observe-integers
  '(for ([x (in-amb/do (ann (amb 1 2) Integer))])
     (write (add1 x))))


;; ---- what the type checker says ----

(test-case "a merger cannot make an answer"
  ;; the example of the review of PR #24: it was accepted when Merger
  ;; was (→ Generator Generator Generator), and add1 got a string
  (check-exn
   ;; a type error, at the answer that the merger makes
   (λ (e)
     (and (exn:fail:syntax? e)
          (regexp-match? #rx"Type Checker" (exn-message e))
          (for/or ([stx (exn:fail:syntax-exprs e)])
            (regexp-match? #rx"wrong" (format "~s" (syntax->datum stx))))))
   (λ ()
     (declare! 'forge 'typed/racket/base
               '(require typed/amb)
               '(define-type Generator (Generatorof (Listof Any)))
               '(: end Generator)
               '(define (end) (values #f end))
               '(: forge Merger)
               '(define (forge s t)
                  (lambda () (values (list "wrong") end)))
               `(parameterize ([current-amb-merger forge])
                  ,observe-integers))))
  ;; nor with the old type, as a merger of generators of any answers
  (check-exn
   #rx"Type Checker"
   (λ ()
     (declare! 'forge2 'typed/racket/base
               '(require typed/amb)
               '(define-type Generator (Generatorof (Listof Any)))
               '(: end Generator)
               '(define (end) (values #f end))
               '(: forge (→ Generator Generator Generator))
               '(define (forge s t)
                  (lambda () (values (list "wrong") end)))
               '(current-amb-merger forge))))
  ;; nor by changing an answer that it was given
  (check-exn
   #rx"Type Checker"
   (λ ()
     (declare! 'forge3 'typed/racket/base
               '(require typed/amb)
               '(: twice Merger)
               '(define ((twice s t))
                  (let-values ([(v s) (s)])
                    (if (list? v)
                        (values (append v v) (twice s t))
                        (values v (twice s t)))))))))

(test-case "what a merger can do"
  ;; drop, repeat, reorder, pause; and the pattern for n pauses
  (check-equal?
   (run! 'ok 'typed/racket/base
         '(require typed/amb typed/amb/merge)
         '(: pauses (∀ (a) (→ Natural (Generatorof a) (Generatorof a))))
         '(define (pauses n g)
            (if (zero? n) g (λ () (values 1 (pauses (sub1 n) g)))))
         '(: my-interleave Merger)
         '(define ((my-interleave s t))
            (let-values ([(v s) (s)])
              (cond
                [(list? v) (values v (my-interleave s t))]
                [(not v) (t)]
                [else (values 1 (my-interleave t (pauses (sub1 v) s)))])))
         '(: tick-append Merger)
         '(define ((tick-append s t)) (values 1 (amb-append s t)))
         '(: second-only Merger)
         '(define (second-only s t) t)
         '(: each-twice Merger)
         '(define ((each-twice s t))
            (let-values ([(v s) (s)])
              (cond
                [(not v) (t)]
                [(exact-positive-integer? v) (values v (each-twice s t))]
                [else (values v (λ () (values v (each-twice s t))))])))
         '(for ([m : Merger (list amb-append amb-interleave amb-alternate amb-level
                                  my-interleave tick-append second-only each-twice)])
            (parameterize ([current-amb-merger m])
              (write (for/list : (Listof (∪ Integer Symbol))
                       ([x (in-amb/do (amb 1 (begin (amb-pause) 2) 3)
                                      #:on-pause (λ () 'P))])
                       x))))
         ;; the value of the parameter is a Merger: it keeps the type of
         ;; the generators it is applied to
         '(define g : (Generatorof (List Integer))
            ((current-amb-merger)
             (λ () (values (list 1) (λ () (values #f (λ () (error "the end"))))))
             (λ () (values #f (λ () (error "the end"))))))
         '(write (let-values ([(v next) (g)]) v)))
   (string-append "(1 P 2 3)" "(1 P 3 2)" "(1 P 3 2)" "(1 3 P 2)"
                  "(1 P 3 2)" "(P 1 P P 2 P 3)" "()" "(1 1 P 2 2 3 3)"
                  "(1)")))


;; ---- mergers from untyped code ----

(test-case "untyped code sets the parameter: nothing is checked"
  ;; the type of the observer is not kept
  (for ([lang '(typed/racket/base typed/racket/base/shallow)])
    (check-exn
     #rx"add1: contract violation.*given: \"wrong\""
     (λ ()
       (run! 'client lang
             '(require typed/amb)
             '(require/typed "untyped-mergers.rkt"
                 [call-with-forge (→ (→ Void) Void)])
             `(call-with-forge (λ () ,observe-integers)))))))

(test-case "dotted contracts and the trusted import boundary"
  ;; Deep require/typed cannot build a contract for dotted polymorphism.
  (check-exn
   #rx"cannot generate contract for variable arity polymorphic type"
   (λ ()
     (declare! 'client 'typed/racket/base
               '(require typed/amb)
               '(require/typed "untyped-mergers.rkt" [first-wins Merger]))))
  ;; A client may explicitly trust its untyped implementation, as the
  ;; library does for the implementations it maintains.
  (check-equal?
   (run! 'client 'typed/racket/base
         '(require typed/amb typed/racket/unsafe)
         '(unsafe-require/typed "untyped-mergers.rkt" [first-wins Merger])
         `(parameterize ([current-amb-merger first-wins]) ,observe-integers))
   "23")
  ;; Protocol checks do not prove that an arbitrary untyped merger
  ;; preserves the types of answers.
  (check-exn
   #rx"add1: contract violation.*given: \"wrong\""
   (λ ()
     (run! 'client 'typed/racket/base
           '(require typed/amb typed/racket/unsafe)
           '(unsafe-require/typed "untyped-mergers.rkt" [forge Merger])
           `(parameterize ([current-amb-merger forge]) ,observe-integers)))))

(test-case "clients in all three modes use the standard mergers"
  (for ([lang '(typed/racket/base typed/racket/base/shallow typed/racket/base/optional)])
    (check-equal?
     (run! 'client lang
           '(require typed/amb typed/amb/merge)
           `(parameterize ([current-amb-merger amb-interleave]) ,observe-integers))
     "23")))
