#lang racket
(require "ast.rkt"
         "util.rkt")
(provide unparse
         unparse-system)

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
    [(hole^ _ _ _ _) 'HOLE]
    [_ (error 'unparse "unsupported form: ~a" exp)]))

;; insert define-structs if there are some
(define (unparse-struct decl)
  (match-define (product$ name fields) decl)
  `(define-struct
     ,(string->symbol (pascal->kebab name))
     (,@(map (compose string->symbol product-field$-name) fields))))

(define (unparse-system sys)
  `(begin
     ,@(for/list ([decl (in-list sys)]
                  #:when (product$? decl))
         (unparse-struct decl))))

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
            [(string=? c "green") "yellow"])))

  (check-equal?
   (unparse-struct
    (product$ "FishtailPalm"
              (list
               (product-field$ "sapwood" "Seeker")
               (product-field$ "duramen" "Tokamak")
               (product-field$ "stump" "Topspin"))))
   '(define-struct fishtail-palm (sapwood duramen stump))))
