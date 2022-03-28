#lang racket
(require "from-checkers/datadef.rkt"
         "../ast.rkt")

(define (fresh-eval x) (eval x (make-base-namespace)))

(define (read-file-with-lang f)
  (match (parameterize ([read-accept-reader #t])
           (call-with-input-file f read))
    [`(module ,_ ,_ (#%module-begin ,xs ...)) xs]))

;; assuming all checks take the form (check-expect (NAME INPUTS ...) OUTPUT)
(define (parse-checks prog fn-name)
  (for/list ([l (in-list prog)]
             #:when (match l
                      [`(check-expect (,(== fn-name) ,_ ...) ,_) #t]
                      [_ #f]))
    (match l
      [`(check-expect ,actual ,expected) (check^ actual expected)])))

(module+ test
  (require rackunit)

  (define mult-numbers-prog (read-file-with-lang "../examples/mult-numbers.rkt")))
