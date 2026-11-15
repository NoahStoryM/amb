#lang racket/base

;; A choice does not keep an alternative that it has supplied: once a
;; thunk of `amb` or of `for/amb #:length` is taken, what only it refers
;; to can be collected, while the other alternatives of the same choice
;; are still to come.  The slots of the vector that the syntax makes
;; are cleared as they are taken (`mutable-vector-generate*` in
;; private/choice.rkt); a vector of a user is not changed.
;;
;; The reference to the big vector must be one that the compiler cannot
;; drop: if the first alternative only computed (vector-length big),
;; that pure expression would be removed, `big` would not be captured at
;; all, and the test would pass for the wrong reason.

(require rackunit
         racket/vector
         amb
         amb/merge
         (only-in "../private/choice.rkt" mutable-vector-generate*))

(define mergers (list amb-append amb-interleave amb-alternate amb-level))

(define sink (box 0))
(define wb #f)

;; a big vector, watched by `wb`
(define (big!)
  (define big (make-vector 1000000 1))
  (set! wb (make-weak-box big))
  big)

;; an alternative that uses `big` and fails
(define (use big)
  (set-box! sink (vector-ref big (unbox sink)))
  (amb-fail))

;; an alternative that reports whether the big vector is still there
(define (probe)
  (collect-garbage)
  (collect-garbage)
  (if (weak-box-value wb) 'retained 'freed))

(define (answers thk) (for/list ([x (in-amb* thk)]) x))

(test-case "a supplied alternative is not retained by its choice"
  (for ([m (in-list mergers)])
    (parameterize ([current-amb-merger m])
      ;; two and three alternatives
      (check-equal? (answers (λ () (let ([big (big!)]) (amb (use big) (probe)))))
                    '(freed))
      (check-equal? (answers (λ () (let ([big (big!)]) (amb (use big) (probe) (probe)))))
                    '(freed freed))
      ;; the first alternative is the only one that refers to `big`
      (check-equal? (answers (λ () (for/amb #:length 2 ([b (in-list (list (big!) 'small))])
                                     (if (eq? b 'small) (probe) (use b)))))
                    '(freed))
      (check-equal? (answers (λ () (for/amb #:length 3 ([b (in-list (list (big!) 'small 'small))])
                                     (if (eq? b 'small) (probe) (use b)))))
                    '(freed freed))
      (check-equal? (answers (λ () (for/amb #:length 2 #:fill (probe)
                                     ([b (in-list (list (big!)))])
                                     (use b))))
                    '(freed))
      ;; the length is larger than the number of iterations
      (check-equal? (answers (λ () (for/amb #:length 5 ([b (in-list (list (big!) 'small))])
                                     (if (eq? b 'small) (probe) (use b)))))
                    '(freed))
      ;; without #:length, the coroutine keeps only the current iteration
      (check-equal? (answers (λ () (for/amb ([b (in-list (list (big!) 'small))])
                                     (if (eq? b 'small) (probe) (use b)))))
                    '(freed)))))

(test-case "the shuffler is still called once, before any slot is taken"
  (define seen '())
  (parameterize ([current-amb-shuffler
                  (λ (v)
                    (set! seen (cons (for/list ([t (in-vector v)]) (procedure? t)) seen))
                    (define x (vector-ref v 0))
                    (vector-set! v 0 (vector-ref v 1))
                    (vector-set! v 1 x))])
    (check-equal? (answers (λ () (amb 1 2 3))) '(2 1 3))
    (check-equal? (answers (λ () (for/amb #:length 3 ([i 2]) i))) '(1 0)))
  (check-equal? seen '((#t #t) (#t #t #t))))

(test-case "a vector of a user is not changed"
  (define thk* (vector (λ () 1) (λ () 2)))
  (define copy (vector-copy thk*))
  (check-equal? (answers (λ () ((sequence->amb thk*)))) '(1 2))
  (check-equal? thk* copy)
  (define v (vector 'a 'b 'c))
  (check-equal? (answers (λ () (sequence->amb v))) '(a b c))
  (check-equal? v #(a b c)))


;; ---- mutable-vector-generate* ----

(define (raw-results gen)
  (let loop ([gen gen])
    (define-values (r next) (gen))
    (if r (cons r (loop next)) '())))

(define called-again #rx"^amb: a generator of a choice was called again")

(test-case "the results are those of sequence-generate*"
  (for ([v (in-list (list (vector) (vector 1) (vector 1 2 3)))])
    (check-equal? (raw-results (λ () (mutable-vector-generate* (vector-copy v))))
                  (raw-results (λ () (sequence-generate* v))))))

(test-case "each slot is cleared when it is taken"
  (define v (vector 'a 'b 'c))
  (define-values (r1 next1) (mutable-vector-generate* v))
  (check-equal? r1 '(a))
  (check-equal? v #(#f b c))
  (define-values (r2 next2) (next1))
  (check-equal? r2 '(b))
  (check-equal? v #(#f #f c))
  (define-values (r3 next3) (next2))
  (check-equal? r3 '(c))
  (check-equal? v #(#f #f #f))
  (define-values (r4 next4) (next3))
  (check-false r4))

(test-case "a generator that is called again is an error"
  (define v (vector 'a 'b 'c))
  (define-values (r1 next1) (mutable-vector-generate* v))
  (define-values (r2 next2) (next1))
  ;; next1 has taken slot 1
  (check-exn (λ (e) (and (exn:fail:contract? e) (regexp-match? called-again (exn-message e))))
             next1)
  ;; the error changes nothing: the live generator goes on
  (check-equal? v #(#f #f c))
  (define-values (r3 next3) (next2))
  (check-equal? r3 '(c))
  (define-values (r4 next4) (next3))
  (check-false r4)
  ;; the generator returned with the last answer has given the end
  (check-exn (λ (e) (and (exn:fail:contract? e) (regexp-match? called-again (exn-message e))))
             next3)
  (check-exn (λ (e) (and (exn:fail:contract? e) (regexp-match? called-again (exn-message e))))
             next2))

(test-case "after the end, the generator is that of sequence-generate*"
  (define (message-after-end gen)
    (define-values (r next) (gen))
    (check-false r)
    (with-handlers ([exn:fail:contract? exn-message]) (next) 'no-error))
  (check-equal? (message-after-end (λ () (mutable-vector-generate* (vector))))
                (message-after-end (λ () (sequence-generate* (vector)))))
  (define-values (r next) (mutable-vector-generate* (vector 'a)))
  (check-equal? (message-after-end next)
                (message-after-end (λ () (sequence-generate* (vector))))))

(test-case "a merger that supplies the rest of a choice twice is stopped"
  ;; it breaks single-pass ownership: it calls the rest of `a`, the
  ;; generator that the choice merges with, twice
  (define (twice a b)
    (λ ()
      (define-values (r next) (a))
      (if r
          (values r (λ () (b) (b)))
          (b))))
  (parameterize ([current-amb-merger twice])
    (check-exn (λ (e) (and (exn:fail:contract? e) (regexp-match? called-again (exn-message e))))
               (λ () (answers (λ () (amb 1 2 3)))))
    (check-exn (λ (e) (and (exn:fail:contract? e) (regexp-match? called-again (exn-message e))))
               (λ () (answers (λ () (for/amb #:length 3 ([i 3]) i)))))))
