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
    [(hole^ can-fill-const? cenv sig checks)
     `(DEBUG-HOLE #;,can-fill-const?
                  #;,cenv
                  #;,sig
                  #;,checks)]
    [_ (error 'unparse "unsupported form: ~a" exp)]))

;; insert define-structs if there are some
(define (unparse-struct decl)
  (match-define (product$ name fields) decl)
  `(define-struct
     ,(string->symbol name)
     (,@(map (compose string->symbol product-field$-name) fields))))

(define (unparse-system sys)
  `(begin
     ,@(let loop ([current-sigs (map defn$-type sys)]
                  [defstructs '()])
         (cond [(empty? current-sigs) defstructs]
               [(product$? (first current-sigs))
                (loop (rest current-sigs)
                      (cons (unparse-struct (first current-sigs))
                            defstructs))]
               [(sum$? (first current-sigs))
                (loop (rest current-sigs)
                      (append defstructs
                              (loop (map sum-case$-type (sum$-cases (first current-sigs)))
                                    '())))]))))

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
    (product$ "fishtail-palm"
              (list
               (product-field$ "sapwood" "Seeker")
               (product-field$ "duramen" "Tokamak")
               (product-field$ "stump" "Topspin"))))
   '(define-struct fishtail-palm (sapwood duramen stump)))

  (define bon-system
    (list
     (defn$ "BunchOfNumbers"
       (sum$ (list (sum-case$ (product$ "none" '()))
                   (sum-case$ (product$ "some"
                                        (list
                                         (product-field$ "first" (number-atom$))
                                         (product-field$ "rest" "BunchOfNumbers")))))))))

  (check-equal?
   (unparse-system bon-system)
   '(begin
      (define-struct some (first rest))
      (define-struct none ()))))
