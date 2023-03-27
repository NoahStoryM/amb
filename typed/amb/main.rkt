#lang typed/racket/base

(require typed/racket/unsafe
         (for-syntax racket/base syntax/parse))

(provide amb for/amb for*/amb)

(require/typed/provide
    "../../amb/private/utils.rkt"
  [raise-amb-error (-> Nothing)]
  [current-amb-shuffler (Parameter (All (A) (-> (Listof A) (Listof A))))]
  [current-amb-tree (Parameter (-> Nothing))]
  [#:struct (exn:fail:amb exn:fail) ()
   #:extra-constructor-name make-exn:fail:amb])

(unsafe-require/typed/provide
 "../../amb/private/utils.rkt"
 [make-amb-tree
  (All (A ...)
       (->* ((-> A ... A Nothing) (Listof (-> (Values A ... A))))
            ((All (A) (-> (Listof A) (Listof A))) (-> Nothing))
            (-> Nothing)))])


(define-syntax amb
  (let ()
    (define alt-parser
      (syntax-parser
        #:datum-literals (amb ann :)
        [(amb alt ...)
         (let ([alt* (syntax->list #'(alt ...))])
           #`(amb #,@(map alt-parser alt*)))]
        [(ann (amb alt ...+) (~optional :) t)
         (with-syntax ([(alt ...) #'((ann alt t) ...)])
           (alt-parser #'(amb alt ...)))]
        [(ann (ann e (~optional :) t1) (~optional :) t2)
         ;; TODO check t1 <=: t2
         #'(ann e t2)]
        [(ann e (~optional :) t) #'(ann e t)]
        [e #'(ann e Nothing)]))
    (λ (stx)
      (syntax-parse stx
        #:datum-literals (amb ann :)
        [(_) #'((current-amb-tree))]
        [(_ alt) #'alt]
        [(_ alt0 ... (amb alt1 ...) alt2 ...)
         #'(amb alt0 ... alt1 ... alt2 ...)]
        [(_ (ann e (~optional :) t) ...+)
         (with-syntax ([t #'(U t ...)])
           #'(let/cc k : t
               (: alt* (Listof (-> t)))
               (define alt* (list (λ () e) ...))
               (define amb-tree (make-amb-tree k alt*))
               (current-amb-tree amb-tree)
               (amb-tree)))]
        [(_ alt0 alt ...+) (alt-parser stx)]))))

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
              #,@(apply append (syntax->list #'(break ...)))
              (ann (ann (λ () body ...) (-> t2)) (-> t1)))]
          [(~or* (name (~optional (~seq : t)) (clause ...) break:break-clause ... body ...+)
                 (name (clause ...) (~optional (~seq : t)) break:break-clause ... body ...+))
           (with-syntax ([t (if (attribute t) #'t #'AnyValues)])
             (parser #'(name : t (clause ...) : t break ... body ...)))]))
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
           (with-syntax ([t
                          (cond
                            [(and (attribute t1) (attribute t2)) #'(U t1 t2)]
                            [(attribute t1) #'t1]
                            [(attribute t2) #'t2]
                            [else #'AnyValues])])
             #`(let/cc k : t
                 #;(: alt* (Listof (-> t)))
                 (define alt* #,((alt*-parser derived-stx) stx))
                 (define amb-tree (make-amb-tree k alt*))
                 (current-amb-tree amb-tree)
                 (amb-tree)))])))
    (values (make-for/amb #'for/list)
            (make-for/amb #'for*/list))))
