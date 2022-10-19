#lang racket
(require bingus
         rackunit)

(define (subst from to l)
  (cond [(empty? l) '()]
        [(list? (first l))
         (cons (subst from to (first l))
               (subst from to (rest l)))]
        [(eq? (first l) from)
         (cons to (subst from to (rest l)))]
        [else (cons (first l) (subst from to (rest l)))]))

(define (alpha-equiv-ish? p1 p2)
  (match-define `(define (,_ ,p1-vars ...) ,_ ...) p1)
  (match-define `(define (,_ ,p2-vars ...) ,_ ...) p2)
  (equal?
   (for/fold ([l p1])
             ([v1 (in-list p1-vars)]
              [v2 (in-list p2-vars)])
     (subst v1 v2 l))
   p2))

(define-check (check-synthesize filename defn)
  (displayln (format "Running test ~a in file ~a" defn filename))
  (define prog (open-input-file (format "tests/~a.rkt" filename)))
  (define actual (time (synthesize prog defn)))
  (define expected-port (open-input-file (format "tests/~a-~a-res.rkt" filename defn)))
  (define expected (read expected-port))
  (close-input-port prog)
  (close-input-port expected-port)
  (unless (alpha-equiv-ish? actual expected)
    (displayln (format "actual: ~a" actual))
    (displayln (format "expected: ~a" expected))
    (fail-check)))

(module+ test
  (check-synthesize "bunch-of-numbers" 'product)
  (check-synthesize "nesting-doll" 'green-doll))
