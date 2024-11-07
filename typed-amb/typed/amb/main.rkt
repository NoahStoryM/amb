#lang typed/racket/base

(require typed/racket/unsafe
         typed/data/queue
         (for-syntax racket/base syntax/parse))

(provide amb amb* for/amb for*/amb in-amb)

(unsafe-require/typed/provide amb
  [in-amb/thunk (∀ (a ...) (→ (→ (Values a ... a)) (Sequenceof a ... a)))])

(unsafe-require/typed/provide amb/private/utils
  [#:struct (exn:fail:contract:amb exn:fail:contract) ()]
  [raise-amb-error (→ Nothing)]
  [current-amb-empty-handler (Parameter (→ Nothing))]
  [current-amb-shuffler (Parameter (∀ (a) (→ (Listof a) (Listof a))))]
  [current-amb-queue    (Parameter (Queue (-> Nothing) (-> Nothing)))]
  [current-amb-enqueue! (Parameter (→ (Queue (-> Nothing) Any) (-> Nothing) Void))]
  [current-amb-dequeue! (Parameter (→ (Queue Nothing (-> Nothing)) (-> Nothing)))]
  [schedule-amb-tasks!  (∀ (a ...) (→ (→ a ... a Nothing) (Listof (→ (Values a ... a))) Void))])


(define-syntax (amb stx)
  (syntax-parse stx
    #:datum-literals ()
    [(_) (syntax/loc stx (amb* ((current-amb-empty-handler))))]
    [(_ alt ...+)
     #:with ooo (datum->syntax #f '...)
     (syntax/loc stx
       (let ()
         (: s&i! (∀ (a ooo) (→ (Listof (→ (Values a ooo a))) (→ (→ a ooo a Nothing) Nothing))))
         (define ((s&i! alt*) k)
           (schedule-amb-tasks! k alt*)
           (((current-amb-dequeue!)
             (current-amb-queue))))
         (call/cc (s&i! (list (λ () alt) ...)))))]))

(define-syntax (amb* stx)
  (syntax-parse stx
    #:datum-literals ()
    [(_ expr ...)
     (syntax/loc stx
       (if (non-empty-queue? (current-amb-queue))
           (((current-amb-dequeue!)
             (current-amb-queue)))
           (values expr ...)))]))


(define-syntaxes (for/amb for*/amb)
  (let ()
    (define-splicing-syntax-class break-clause
      [pattern (~seq (~or* #:break #:final) guard:expr)])
    (define (alt*-parser derived-stx)
      (define parser
        (syntax-parser
          #:datum-literals (:)
          [(_ : t1 (clause ...) : t2 break:break-clause ... body ...+)
           #`(#,derived-stx
              : (Listof (→ t1))
              (clause ...)
              : (Listof (→ t2))
              break ...
              (ann (ann (λ () body ...) (→ t2)) (→ t1)))]
          [(~or* (name : t0 (clause ...) break:break-clause ... body ...+)
                 (name (clause ...) : t0 break:break-clause ... body ...+)
                 (name (clause ...) break:break-clause ... body ...+))
           #:with t (if (attribute t0) #'t0 #'AnyValues)
           (parser #'(name : t (clause ...) : t break ... body ...))]))
      parser)
    (define ((make-for/amb derived-stx) stx)
      (syntax-parse stx
        #:datum-literals (:)
        [(_ (~optional (~seq : t1))
            (clause ...)
            (~optional (~seq : t2))
            break:break-clause ...
            body ...+)
         #:with t
         (cond
           [(and (attribute t1) (attribute t2)) #'(∪ t1 t2)]
           [(attribute t1) #'t1]
           [(attribute t2) #'t2]
           [else #'AnyValues])
         (quasisyntax/loc stx
           (let/cc k : t
             #;(: alt* (Listof (→ t)))
             (define alt* #,((alt*-parser derived-stx) stx))
             (schedule-amb-tasks! k alt*)
             (amb)))]))
    (values (make-for/amb #'for/list)
            (make-for/amb #'for*/list))))


(define-syntax (in-amb stx)
  (syntax-parse stx
    #:datum-literals ()
    [(_ expr) (syntax/loc stx (in-amb/thunk (λ () expr)))]))
