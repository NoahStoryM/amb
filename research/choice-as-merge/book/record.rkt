#lang racket/base

;; Runs the examples of the book one by one and records, for each, how
;; the result compares with the result printed in the book.

(require file/sha1
         racket/list)

(provide record! finish!)

(define timeout-seconds
  (string->number (or (getenv "BOOK_TIMEOUT") "120")))
(define memory-limit (* 3 1024 1024 1024))

;; (list location class milliseconds actual), most recent first
(define records '())

;; A result is recorded as it is, unless it is long: then as its length
;; and a digest, which is enough to compare two runs.
(define (compact v)
  (define text (format "~s" v))
  (if (< (string-length text) 4000)
      v
      (list 'digest
            (if (list? v) (length v) #f)
            (sha1 (open-input-string text)))))

(define (bag l) (sort l string<? #:key (λ (a) (format "~s" a))))

;;   same          the result of the book
;;   same-answers  the same answers in another order
;;   same-count    as many answers, but not the same ones
;;   different     anything else
;;   timeout       no result within the time or memory limit
(define (classify actual expected)
  (cond
    [(equal? actual expected) 'same]
    [(not (and (list? actual) (list? expected))) 'different]
    [(equal? (bag actual) (bag expected)) 'same-answers]
    [(= (length actual) (length expected)) 'same-count]
    [else 'different]))

(define (record! location actual-thunk expected)
  (define result (box #f))
  (define cust (make-custodian))
  (custodian-limit-memory cust memory-limit)
  (define t0 (current-inexact-milliseconds))
  (define worker
    (parameterize ([current-custodian cust])
      (thread
       (λ ()
         (set-box! result
                   (with-handlers ([exn:fail? (λ (e) (list 'error (exn-message e)))])
                     (list 'ok (actual-thunk))))))))
  (sync/timeout timeout-seconds worker)
  (custodian-shutdown-all cust)
  (define ms (round (- (current-inexact-milliseconds) t0)))
  (define-values (class actual)
    (let ([r (unbox result)])
      (cond
        [(not r) (values 'timeout #f)]
        [(eq? (car r) 'error) (values 'error (cadr r))]
        [else (values (classify (cadr r) expected) (cadr r))])))
  (unless (eq? class 'same)
    (printf "  ~a: ~a (~a ms)\n" location class ms)
    (flush-output))
  (set! records (cons (list location class ms (compact actual)) records)))

;; Prints a summary and writes the records to `file`.
(define (finish! name file)
  (define all (reverse records))
  (define total-ms (apply + (map caddr all)))
  (printf "~a: ~a examples, ~a s\n" name (length all) (/ (round (/ total-ms 100.0)) 10.0))
  (for ([class '(same same-answers same-count different timeout error)])
    (define n (count (λ (r) (eq? (cadr r) class)) all))
    (unless (zero? n) (printf "  ~a ~a\n" (~r n) class)))
  (when file
    (with-output-to-file file #:exists 'truncate
      (λ ()
        (printf "(\n")
        (for ([r all]) (printf " ~s\n" r))
        (printf ")\n")))))

(define (~r n) (let ([s (number->string n)]) (string-append (make-string (max 0 (- 4 (string-length s))) #\space) s)))
