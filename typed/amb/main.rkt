#lang typed/racket/base

(require "../data/queue.rkt"
         typed/racket/unsafe
         (for-syntax racket/base syntax/parse))

(provide amb amb* for/amb for*/amb)

(require/typed/provide "../../amb/private/utils.rkt"
  [#:struct (exn:fail:contract:amb exn:fail:contract) ()])

(unsafe-require/typed/provide "../../amb/private/utils.rkt"
  [current-amb-shuffler (Parameter (All (A) (-> (Listof A) (Listof A))))]
  [current-amb-queue    (Parameter (Queue (-> Nothing) (-> Nothing)))]
  [current-amb-enqueue! (Parameter (-> (Queue (-> Nothing) (-> Nothing)) (-> Nothing) Void))]
  [current-amb-dequeue! (Parameter (-> (Queue (-> Nothing) (-> Nothing)) (-> Nothing)))]
  [insert-amb-node*! (All (A ...) (-> (-> A ... A Nothing) (Listof (-> (Values A ... A))) Void))])


(define-syntax amb
  (λ (stx)
    (syntax-parse stx
      #:datum-literals (amb :)
      [(_) #'(amb* (raise (exn:fail:contract:amb
                           "amb: empty amb queue;\n expected at least one amb node\n  in: (amb)"
                           (current-continuation-marks))))]
      [(_ : t) #'(ann (amb) t)]
      [(_ : t0 alt0 ... (amb : t1 alt1 ...) alt2 ...)
       #'(amb : (∪ t0 t1) alt0 ... alt1 ... alt2 ...)]
      [(_ : t alt0 ... (amb alt1 ...) alt2 ...)
       #'(amb : t alt0 ... alt1 ... alt2 ...)]
      [(_ : t alt ...+)
       #'(let/cc k : t
           (: alt* (Listof (-> t)))
           (define alt* (list (λ () alt) ...))
           (insert-amb-node*! k alt*)
           (amb : t))]
      [(_ alt ...+)
       #'(amb : AnyValues alt ...)])))

(define-syntax amb*
  (syntax-parser
    #:datum-literals ()
    [(_ v ...)
     #'(if (queue-empty? (current-amb-queue))
           (values v ...)
           (((current-amb-dequeue!)
             (current-amb-queue))))]))

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
              : (Listof (-> t1))
              (clause ...)
              : (Listof (-> t2))
              break ...
              (ann (ann (λ () body ...) (-> t2)) (-> t1)))]
          [(~or* (name : t0 (clause ...) break:break-clause ... body ...+)
                 (name (clause ...) : t0 break:break-clause ... body ...+)
                 (name (clause ...) break:break-clause ... body ...+))
           #:with t
           (if (attribute t0) #'t0 #'AnyValues)
           (parser #'(name : t (clause ...) : t break ... body ...))]))
      parser)
    (define (make-for/amb derived-stx)
      (λ (stx)
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
           #`(let/cc k : t
               #;(: alt* (Listof (-> t)))
               (define alt* #,((alt*-parser derived-stx) stx))
               (insert-amb-node*! k alt*)
               (amb : t))])))
    (values (make-for/amb #'for/list)
            (make-for/amb #'for*/list))))
