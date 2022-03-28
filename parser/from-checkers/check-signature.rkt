#lang racket
(provide signature
         ss->signature
         signature-exists?
         signature-correct?)

(require "data-def-parsing.rkt")

; A Signature is (signature Symbol [ListOf Symbol] Symbol)
(struct signature (name args return-type)
  #:transparent)

; transform : String -> Symbol
(define transform (compose string->symbol string-downcase))

; check-signature : Signature ListofSignature ListofDataDefinition -> Boolean
(define (check-signature s correct-env dds)
  (type-check-function (reduce-signature dds s) correct-env))

; reduce-signature : ListofDataDefinition Signature -> Signature
(define (reduce-signature dds s)
  (let ([args^ (map (curry reduce-type dds) (signature-args s))]
        [return-type^ (reduce-type dds (signature-return-type s))])
    (signature (signature-name s) args^ return-type^)))

; reduce-type : ListofDataDefinition Symbol -> Symbol
(define (reduce-type env t)
  (reduce-type/a env t '()))

; reduce-type/a : ListofDataDefinition Symbol ListofSymbol -> Symbol
(define (reduce-type/a env t seen)
  (cond
    [(memv t seen) t] ; cycle among data definitions
    [(memv t '(number boolean image string naturalnumber listofnumber listofboolean))
     t]
    [(dict-has-key? env t)
     (reduce-type/a env (dict-ref env t) (cons t seen))]
    [else t]))

; ss->signature : String -> Signature
(define (ss->signature ss)
  (let* ([res (regexp-match #px"(?m:^[\\s;]*([^][(){}\",'`|;#\\s]+)\\s*:\\s*(.*?)\\s*--*>\\s*(.*?)\\s*$)" ss)]
         [name (second res)]
         [arg-types (string-split (third res))]
         [rt-type (fourth res)])
    (signature (string->symbol name) (map transform arg-types) (transform rt-type))))
    
; type-check-function : Signature ListofSignature -> Boolean
(define (type-check-function signature* correct-env)
  (equal? signature* (get-signature (signature-name signature*) correct-env)))

; get-signature : Symbol ListofSignature -> [Either #f Signature]
(define (get-signature name env)
  (cond
    [(empty? env) #f]
    [else (if (equal? name (signature-name (first env)))
              (first env)
              (get-signature name (rest env)))]))

; signature-exists? : Symbol ListofSignature -> Boolean
(define (signature-exists? f env)
  (if (get-signature f env)
      #t #f))

; signature-correct? : String ListofDataDefinition ListofSignature ListofSignature -> Boolean
(define (signature-correct? f dds parsed-env correct-env)
  (check-signature (get-signature f parsed-env) correct-env dds))
