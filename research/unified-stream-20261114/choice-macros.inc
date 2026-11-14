(define-syntax amb
  (syntax-rules ()
    [(_) (amb-fail)]
    [(_ e ...) (amb* (current-amb-merge) (vector (λ () e) ...))]))

;; (for/amb (clause ...) body ...) is a choice among the values of the
;; body, one alternative for each iteration; the body runs where the
;; choice was made.
;;
;;   with #:length n   the alternatives are collected in a vector first,
;;                     as by for/vector (at most n of them), and the
;;                     choice is that of `amb*`
;;   without           the iterations go on as the search does, and the
;;                     choice is that of `amb-from`
(define-syntaxes (for/amb for*/amb)
  (let ()
    (define-splicing-syntax-class break
      [pattern (~seq (~or* #:break #:final) guard:expr)])
    (define ((expander for/vector for/stream) stx)
      (syntax-parse stx
        [(_ #:length n (clause ...) b:break ... body ...+)
         #:with (b* ...) #'((~@ . b) ...)
         (quasisyntax/loc stx
           (amb* (current-amb-merge)
                 (trim (#,for/vector #:length n #:fill #f (clause ...) b* ...
                        (λ () body ...)))))]
        [(_ (clause ...) b:break ... body ...+)
         #:with (b* ...) #'((~@ . b) ...)
         (quasisyntax/loc stx
           ((amb-from (#,for/stream (clause ...) b* ... (λ () body ...)))))]))
    (values (expander #'for/vector #'for/stream)
            (expander #'for*/vector #'for*/stream))))

;; The vector without the slots at its end that were not filled.
(define (trim alts)
  (let loop ([n (vector-length alts)])
    (cond
      [(and (positive? n) (not (vector-ref alts (sub1 n)))) (loop (sub1 n))]
      [(= n (vector-length alts)) alts]
      [else
       (let ([v (make-vector n #f)])
         (vector-copy! v 0 alts 0 n)
         v)])))
