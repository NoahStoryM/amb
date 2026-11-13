#lang racket/base

;; Reads the records in results/ and prints, for each run, how its
;; results compare with the book; then compares two runs with each other,
;; example by example:  racket compare.rkt [run1 run2]

(require racket/list
         racket/format)

(define (read-records name)
  (define file (build-path "results" (string-append name ".rktd")))
  (and (file-exists? file) (call-with-input-file file read)))

(define names
  '("reference" "interleave+local" "interleave" "alternate" "level"
    "ambkanren" "ambkanren-lawful"))
(define classes '(same same-answers same-count different timeout error))

(printf "~a ~a ~a\n" (~a "" #:min-width 18)
        (apply string-append (for/list ([c classes]) (~a c #:min-width 13 #:align 'right)))
        (~a "seconds" #:min-width 9 #:align 'right))
(for ([name names])
  (define records (read-records name))
  (when records
    (printf "~a ~a ~a\n" (~a name #:min-width 18)
            (apply string-append
                   (for/list ([c classes])
                     (define n (count (λ (r) (eq? (cadr r) c)) records))
                     (~a (if (zero? n) "" n) #:min-width 13 #:align 'right)))
            (~a (~r (/ (apply + (map caddr records)) 1000.0) #:precision '(= 1))
                #:min-width 9 #:align 'right))))

(define (compare name1 name2)
  (define records1 (read-records name1))
  (define records2 (read-records name2))
  (printf "\n~a and ~a:\n" name1 name2)
  (define differ
    (for/list ([r1 records1] [r2 records2]
               #:unless (equal? (cadddr r1) (cadddr r2)))
      (unless (equal? (car r1) (car r2)) (error 'compare "records out of step"))
      (list (car r1) (cadr r1) (cadr r2))))
  (printf "  ~a of ~a examples have the same result\n"
          (- (length records1) (length differ)) (length records1))
  (for ([d differ])
    (printf "  ~a: ~a / ~a\n" (car d) (cadr d) (caddr d))))

(define args (current-command-line-arguments))
(cond
  [(= (vector-length args) 2) (compare (vector-ref args 0) (vector-ref args 1))]
  [else
   (compare "interleave" "interleave+local")
   (compare "level" "ambkanren")
   (compare "level" "ambkanren-lawful")])
