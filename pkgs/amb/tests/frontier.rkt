#lang racket/base

;; First-class frontiers: `make-amb-frontier`, `current-amb-frontier`,
;; `amb-frontier-length`, `in-amb #:frontier`.
(require rackunit racket/stream racket/list racket/sequence amb)

(define (pause) (when (amb #t #f) (amb)))

(test-case "current-amb-frontier: #f outside, the root inside, a fresh one in amb/local"
  (check-false (current-amb-frontier))
  (define seen
    (stream->list
     (in-amb (let* ([root (current-amb-frontier)]
                    [local (amb/local (current-amb-frontier))]
                    [after (current-amb-frontier)])
               (list (amb-frontier? root)
                     (amb-frontier? local)
                     (eq? root local)
                     (eq? root after))))))
  (check-equal? seen '((#t #t #f #t)))
  (check-false (current-amb-frontier)))

(test-case "the parameter rejects anything but a frontier or #f"
  (check-exn exn:fail:contract?
             (λ () (parameterize ([current-amb-frontier 'x]) 1))))

(test-case "an inner search does not see the frontier of an outer amb/local"
  (check-equal?
   (stream->list
    (in-amb (amb/local
             (let ([outer (current-amb-frontier)])
               (stream->list
                (in-amb (let ([inner (current-amb-frontier)])
                          (list (amb-frontier? inner) (eq? inner outer)))))))))
   '(((#t #f)))))

(test-case "a local frontier inherits the empty handler, or has its own"
  (define (h) 'R)
  (check-equal? (stream->list
                 (in-amb (amb/local (eq? (amb-frontier-empty-handler (current-amb-frontier)) h))
                         #:empty-handler h))
                '(#t))
  (check-equal? (stream->list
                 (in-amb (amb 1
                              (parameterize ([current-amb-frontier (make-amb-frontier (λ () 'L))])
                                (amb 2 (amb) 3))
                              (amb)
                              4)
                         #:empty-handler (λ () 'R)))
                '(1 2 L R 4 3))
  ;; (the local frontier goes to the back after its turn ends at (amb))
  ;; made outside any search: the default handler, which outputs nothing
  (define f (make-amb-frontier))
  (check-equal? (stream->list
                 (in-amb (parameterize ([current-amb-frontier f]) (amb 1 (amb) 2))
                         #:empty-handler (λ () 'R)))
                '(1 2)))

(test-case "running out is a dead end, never an (amb)"
  ;; the handler is called only for the explicit (amb)
  (define calls 0)
  (check-equal? (stream->list
                 (in-amb (amb/local (amb/local 1 2) (amb/local) 3)
                         #:empty-handler (λ () (set! calls (add1 calls)) 'P)))
                '(1 2 P 3))
  (check-equal? calls 1))

(test-case "amb-frontier-length: the items waiting, a local frontier counts as one"
  (define (len) (amb-frontier-length))
  (check-exn exn:fail:contract? (λ () (amb-frontier-length)))
  ;; the running tree is not counted; the root tree is (it is put back)
  (check-equal? (stream->list (in-amb (len))) '(0))
  (check-equal? (stream->list (in-amb (amb (len) (len) (len)))) '(1 1 1))
  (check-equal? (stream->list (in-amb (list (amb 'a 'b) (len)))) '((a 1) (b 1)))
  ;; inside a local frontier: its own items; around it: one item for it
  (check-equal? (stream->list
                 (in-amb (amb/local (list (len) (amb 'x 'y)))))
                '((0 x) (0 y)))
  (define root #f)
  (check-equal? (stream->list
                 (in-amb (begin
                           (set! root (current-amb-frontier))
                           (amb/local (amb 'p (begin (amb 1 2 3)
                                                     (amb-frontier-length root)))))))
                '(p 1 1 1)))

(test-case "#:frontier: the search runs a given frontier, with its handler"
  (define f (make-amb-frontier (λ () 'F)))
  (check-equal? (stream->list (in-amb (amb 1 (amb) 2) #:frontier f)) '(1 F 2))
  (check-equal? (for/list ([x (in-amb (amb 1 (amb) 2) #:frontier f)]) x) '(1 F 2))
  (check-equal? (for/list ([x (in-amb/do (amb 1 (amb) 2) #:frontier f)]) x) '(1 F 2))
  (check-equal? (for/list ([x (in-amb* (λ () (amb 1 (amb) 2)) #:frontier f)]) x) '(1 F 2))
  (check-equal? (stream->list (in-amb* (λ () (amb 1 (amb) 2)) #:frontier f)) '(1 F 2))
  (check-equal? (for/list ([x (in-amb*/do (λ () (amb 1 (amb) 2)) #:frontier f)]) x) '(1 F 2))
  (check-equal? (amb-frontier-length f) 0)
  ;; not both
  (check-exn #rx"cannot both"
             (λ () (stream->list (in-amb* (λ () 1) (λ () 'h) #:frontier f))))
  (check-exn exn:fail:contract? (λ () (in-amb* (λ () 1) #:frontier 'f))))

(test-case "a frontier shared by two searches: their trees take turns in it"
  (define f (make-amb-frontier))
  ;; the second search, started inside the first, runs the same items:
  ;; here what is left of the first one's root (the tree being resumed
  ;; is not in the frontier while it runs, so 'b comes after)
  (check-equal?
   (stream->list
    (in-amb (amb 'a
                 (stream->list (in-amb (amb 1 2) #:frontier f))
                 'b)
            #:frontier f))
   '(a (1 2) b))
  ;; the choice being resumed is not in the frontier, so the second
  ;; search does not resume it
  (define log '())
  (define g (make-amb-frontier))
  (check-equal?
   (stream->list
    (in-amb (let ([x (amb 'x 'y)])
              (set! log (cons x log))
              (if (eq? x 'x)
                  (length (stream->list (in-amb (amb 1 2) #:frontier g)))
                  'done))
            #:frontier g))
   '(2 done))
  (check-equal? (reverse log) '(x y)))

(test-case "a frontier used at two places is mounted at both"
  (define f (make-amb-frontier (λ () 'P)))
  (define-syntax-rule (in-f e) (parameterize ([current-amb-frontier f]) e))
  ;; both places put their trees into f, which takes turns as one item
  (check-equal?
   (stream->list
    (in-amb (amb/local (in-f (amb 1 (amb) 2))
                       (in-f (amb 'x (amb) 'y)))))
   '(1 P x P 2 y)))

(test-case "regression: a continuation captured in a local frontier, applied again across another prompt"
  ;; Racket v9.3 CS can bring a mark on a prompt's own frame back
  ;; innermost when a continuation captured across another prompt is
  ;; applied again; the frontiers and handlers seen there must not change.
  (define tag (make-continuation-prompt-tag 'other))
  (define saved #f)
  (define L (make-amb-frontier (λ () 'L)))
  (define (probe)
    (list (eq? (current-amb-frontier) L)
          ((amb-frontier-empty-handler (current-amb-frontier)))
          ;; an inner search started here has its own root and handler
          (stream->list (in-amb (list (eq? (current-amb-frontier) L) (amb 1 (amb)))
                                #:empty-handler (λ () 'I)))))
  (define results
    (stream->list
     (in-amb (amb (call-with-continuation-prompt
                   (λ ()
                     (parameterize ([current-amb-frontier L])
                       ((call-with-composable-continuation
                         (λ (k) (set! saved k) values)
                         tag))
                       (probe)))
                   tag)
                  (call-with-continuation-prompt (λ () (saved (λ () 'again))) tag)
                  (call-with-continuation-prompt (λ () (saved (λ () 'again))) tag)
                  (begin (pause) (call-with-continuation-prompt (λ () (saved values)) tag)))
             #:empty-handler (λ () 'R))))
  (define expected '(#t L ((#f 1) I)))
  (check-equal? results (list expected expected expected 'R expected))
  ;; and the same through amb, where the probe is (amb) itself
  (define saved2 #f)
  (check-equal?
   (stream->list
    (in-amb (amb (call-with-continuation-prompt
                  (λ ()
                    (parameterize ([current-amb-frontier L])
                      (call-with-composable-continuation (λ (k) (set! saved2 k)) tag)
                      (amb 'first (amb))))
                  tag)
                 (call-with-continuation-prompt (λ () (saved2 #f)) tag)
                 (begin (pause) (call-with-continuation-prompt (λ () (saved2 #f)) tag)))
            #:empty-handler (λ () 'R)))
   '(first L first L R first L)))

(test-case "the code after amb/local returns is back in the frontier around it"
  (define root #f)
  (check-equal?
   (stream->list
    (in-amb (begin (set! root (current-amb-frontier))
                   (let ([x (amb/local 1 2)])
                     (list x (eq? (current-amb-frontier) root))))))
   '((1 #t) (2 #t))))

(test-case "a choice after an amb/local whose frontier was collapsed"
  ;; the outer local frontier is down to one mounted frontier when the
  ;; inner amb/local returns and a plain choice is made in it
  (define r
    (stream->list
     (in-amb (amb/local 'x
                        (let ([v (amb/local 'a (begin (pause) 'b))])
                          (list v (amb 1 2))))
             #:empty-handler (λ () 'P))))
  (check-equal? (sort (filter pair? r) string<? #:key (λ (p) (format "~a" p)))
                '((a 1) (a 2) (b 1) (b 2)))
  (check-equal? (count (λ (v) (eq? v 'P)) r) 1)
  (check-equal? (car r) 'x))

(test-case "after the handler raises inside a local frontier the search goes on"
  (define first? #t)
  (define-values (more? get)
    (sequence-generate
     (in-amb/do (amb/local (amb/local 1 (amb) 2) 3)
                #:empty-handler (λ () (if first? (begin (set! first? #f) (error "boom")) 'P)))))
  (check-equal? (get) 1)
  (check-exn #rx"boom" get)
  ;; the raise ended nothing: (amb) was used up, and no turn ended there
  (check-equal? (for/list ([_ 2]) (get)) '(2 3))
  (check-false (more?)))

;; ---- From the review in #11 ----

(define-syntax-rule (in g e) (parameterize ([current-amb-frontier g]) e))

(test-case "regression: the same, with amb/local in tail position of the other prompt's thunk"
  ;; amb/local's mark lands on the prompt's own frame.  Replaying the
  ;; continuation, here, in another alternative and under another
  ;; amb/local, must see that frontier (as a plain mark would be seen),
  ;; and a nested amb/local must still get its own.
  (define tag (make-continuation-prompt-tag 'other))
  (define saved #f)
  (define L0 #f)
  (define root #f)
  (check-equal?
   (stream->list
    (in-amb (begin
              (set! root (current-amb-frontier))
              (amb (call-with-continuation-prompt
                    (λ ()
                      (amb/local
                       (begin
                         (call-with-composable-continuation (λ (c) (set! saved c)) tag)
                         (let ([here (current-amb-frontier)])
                           (unless L0 (set! L0 here))
                           (list (cond [(eq? here L0) 'L0] [(eq? here root) 'root] [else 'other])
                                 (amb/local (eq? (current-amb-frontier) here)))))
                       'second))
                    tag)
                   (call-with-continuation-prompt (λ () (saved #f)) tag)
                   (amb/local (call-with-continuation-prompt (λ () (saved #f)) tag))))))
   '((L0 #f) second (L0 #f) (L0 #f))))

(test-case "a frontier installed in another search is not seen, even with a shared root"
  (define R (make-amb-frontier))
  (define g (make-amb-frontier))
  (check-equal?
   (stream->list
    (in-amb (in g (stream->list (in-amb (eq? (current-amb-frontier) g) #:frontier R)))
            #:frontier R))
   '((#f))))

(test-case "installing #f undoes an installation"
  (define g (make-amb-frontier))
  (check-equal?
   (stream->list
    (in-amb (let ([root (current-amb-frontier)])
              (in g (list (eq? (current-amb-frontier) g)
                          (in #f (eq? (current-amb-frontier) root)))))))
   '((#t #t))))

(test-case "a continuation that carries the parameterize loses it under another amb/local"
  ;; documented: unlike other parameters
  (define tag (make-continuation-prompt-tag))
  (define saved #f)
  (define g (make-amb-frontier))
  (define p (make-parameter 'out))
  (check-equal?
   (stream->list
    (in-amb
     (amb (amb/local
           (call-with-continuation-prompt
            (λ () (parameterize ([current-amb-frontier g] [p 'in])
                    ((call-with-composable-continuation (λ (k) (set! saved k) values) tag))
                    (list (eq? (current-amb-frontier) g) (p))))
            tag)
           'x)
          (amb/local (call-with-continuation-prompt (λ () (saved void)) tag) 'y))))
   '((#t in) x (#f in) y)))

(test-case "a reused frontier is not collapsed: the result does not depend on the optimization"
  (define (ticks name) (amb name (begin (pause) (ticks name))))
  (define g (make-amb-frontier))
  (check-equal?
   (for/list ([x (in-amb (amb/local (in g (amb (amb/local (ticks 'a) (ticks 'b))))
                                    (begin (pause) (pause) (in g (amb (ticks 'c) (ticks 'd))))))]
              [_ 12])
     x)
   ;; what the driver gives without collapsing
   '(a b a c d b c d a c d b)))

(test-case "the turn ends where the handler returns"
  ;; a handler that fails does not end a turn; the default one does
  (define (s1) (amb/local 1 2 3 (amb) 4 5 6))
  (define (s2) (amb/local 7 8 (amb) 9 (amb)))
  (check-equal? (stream->list (in-amb (amb/local (s1) (s2)))) '(1 2 3 7 8 4 5 6 9))
  (check-equal? (stream->list (in-amb (amb/local (s1) (s2)) #:empty-handler (λ () (amb-fail))))
                '(1 2 3 4 5 6 7 8 9)))

(test-case "an escape in a search nested on the same frontier: the outer one loses nothing"
  (define f (make-amb-frontier))
  (define first? #t)
  (define-values (more? get)
    (sequence-generate
     (in-amb/do (let ([x (amb 'z 'a 'b 'c)])
                  (when (and first? (eq? x 'a))
                    (set! first? #f)
                    (for ([y (in-amb/do (amb 'i0 (error "boom") 'i1 'i2) #:frontier f)]) (void)))
                  x)
                #:frontier f)))
  (define (step) (with-handlers ([exn:fail? exn-message]) (if (more?) (get) 'end)))
  ;; the inner search's trees were left in the shared frontier, so the
  ;; outer one runs them too
  (check-equal? (for/list ([_ 7]) (step)) '(z "boom" b c i1 i2 end)))

(test-case "an escape in the first step does not push the root again"
  (define f (make-amb-frontier))
  (define n 0)
  (define-values (more? get)
    (sequence-generate
     (in-amb/do (begin (set! n (add1 n)) (when (= n 1) (error "first")) (amb 1 2))
                #:frontier f)))
  (define (step) (with-handlers ([exn:fail? exn-message]) (if (more?) (get) 'end)))
  ;; the computation raised, which uses it up, as for any alternative
  (check-equal? (for/list ([_ 2]) (step)) '("first" end))
  (check-equal? n 1)
  (check-equal? (amb-frontier-length f) 0))

(test-case "an abandoned search leaves its trees in a given frontier"
  (define f (make-amb-frontier))
  (check-equal? (for/first ([x (in-amb/do (amb 1 2 3) #:frontier f)]) x) 1)
  (check-equal? (for/list ([x (in-amb/do (amb 'p 'q) #:frontier f)]) x) '(p q 2 3)))
