#lang racket
(require "ast.rkt"
         "util.rkt")
(provide unparse)

(define (unparse exp)
  (match exp
    [(lambda^ formals body)
     `(define (func ,@formals) ,body)]
    [(? fail?) (error 'unparse "fail")]
    [y y]
    [_ (error 'unparse "unsupported form: ~a" exp)]))
