#lang racket
(require "ast.rkt"
         "util.rkt")
(provide unparse)

(define (unparse exp)
  (match exp
    [y #:when (or (symbol? y)
                  (number? y)
                  (string? y)
                  (boolean? y))
       y]
    [(lambda^ formals body)
     `(define (func ,@formals) ,(unparse body))]
    [(app^ rator rand)
     `(,(unparse rator) ,@(map unparse rand))]
    [(cond^ clauses)
     `(cond ,@(map unparse clauses))]
    [(cond-case^ question answer)
     `(,(unparse question) ,(unparse answer))]
    [_ (error 'unparse "unsupported form: ~a" exp)]))

(module+ test
  (require rackunit)

  (check-equal?
   (unparse
    (lambda^ '(tl)
             (cond^ (list
                     (cond-case^ (app^ 'string=? (list 'c "red")) "green")
                     (cond-case^ (app^ 'string=? (list 'c "yellow")) "red")
                     (cond-case^ (app^ 'string=? (list 'c "green")) "yellow")))))
   '(define (func tl)
      (cond [(string=? c "red") "green"]
            [(string=? c "yellow") "red"]
            [(string=? c "green") "yellow"]))))
