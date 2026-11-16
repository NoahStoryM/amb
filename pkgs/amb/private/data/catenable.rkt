#lang racket/base

;; ===================================================================
;; Lists that are appended in constant time
;; ===================================================================
;;
;; The catenable lists of Okasaki, Purely Functional Data Structures
;; (CMU-CS-96-177, 1996, section 7.2.1; section 10.2.1 of the book).
;;
;; A list that is not empty is a tree.  Its first element is at the root,
;; and the lists that were appended after it are the children of the
;; root, in a queue:
;;
;;   append   adds a child to the root;
;;   rest     removes the root and links its children into one list: the
;;            first child becomes the root, and the link of the other
;;            children becomes its last child.  That link is delayed
;;            until it is itself the first child of a root.
;;
;; Theorem 7.1 there: `append` and `rest` take constant amortized time,
;; counting an operation of the queue as one step.  The queue here is a
;; pair of lists, and what it takes more is paid for once: an element is
;; reversed once, as long as no queue is used twice; and none is, when
;; no list is used twice.  (`append` and `rest` here are Figure 7.5,
;; except that a child that is already built is stored as it is.)
;;
;; Nothing here is mutated except the promises of the delayed links, so a
;; list that is used twice still has the same elements.  Only the bound
;; on the time needs every list to be used once.

(require racket/promise)

(provide cat-of cat-append cat-first cat-rest
         cat->list
         cat-steps)

;; The empty list is '().
;;   first   the first element
;;   kids    a queue of lists, none empty; a list may be a promise
(struct cat (first kids))

(define (cat-of x) (cat x empty-queue))

(define (cat-append a b)
  (cond
    [(null? a) b]
    [(null? b) a]
    [else (cat (cat-first a) (queue-add (cat-kids a) b))]))

(define (cat-rest a)
  (link-all (cat-kids a)))

(define (link-all q)
  (cond
    [(queue-empty? q) '()]
    [else
     (step!)
     (define-values (kid q*) (queue-remove q))
     (let ([kid (force kid)])
       (if (queue-empty? q*)
           kid
           (cat (cat-first kid)
                (queue-add (cat-kids kid) (delay (link-all q*))))))]))

(define (cat->list a)
  (if (null? a)
      '()
      (cons (cat-first a) (cat->list (cat-rest a)))))


;; ---- A queue: the front, and the rear in reverse ----

(define empty-queue '(() . ()))

(define (queue-empty? q)
  (and (null? (car q)) (null? (cdr q))))

(define (queue-add q x)
  (cons (car q) (cons x (cdr q))))

;; The first element and the rest, of a queue that is not empty.
(define (queue-remove q)
  (cond
    [(pair? (car q)) (values (caar q) (cons (cdar q) (cdr q)))]
    [else
     (let ([front (reverse (cdr q))])
       (step! (length front))
       (values (car front) (cons (cdr front) '())))]))


;; ---- Counting the work ----

;; The number of steps taken so far by `cat-rest`: one for each link, and
;; one for each element of a queue that is reversed.  `cat-of` and
;; `cat-append` take a constant number of steps and are not counted.
(define steps 0)
(define (step! [n 1]) (set! steps (+ steps n)))
(define (cat-steps) steps)
