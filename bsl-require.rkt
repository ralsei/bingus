#lang racket
(require racket/sandbox)
(provide eval/bsl
         dynamic-require/bsl)

(define (eval/bsl definitions to-run)
  ((make-module-evaluator `(module m lang/htdp-beginner
                             ,@definitions))
   to-run))

;; thanks alex knauth
(define src-ns (make-base-namespace))
(eval '(require lang/htdp-beginner) src-ns)

;; dynamic-require/bsl : Symbol -> Any
(define (dynamic-require/bsl x)
  (with-handlers ([exn:fail:syntax/bsl-function?
                   (λ (e) (dynamic-require/bsl-function x))])
    (dynamic-require 'lang/htdp-beginner x)))

;; dynamic-require/bsl-function : Symbol -> Procedure
(define (dynamic-require/bsl-function x)
  (lambda args
    (define tmps (map (λ (_) (gensym)) args))
    (define ns (make-base-empty-namespace))
    (for-each (λ (t a) (namespace-set-variable-value! t a #f ns)) tmps args)
    (namespace-attach-module src-ns 'lang/htdp-beginner ns)
    (namespace-require 'lang/htdp-beginner ns)
    (eval (cons x tmps) ns)))

;; exn:fail:syntax/bsl-function? : Any -> Boolean
(define (exn:fail:syntax/bsl-function? v)
  (and
   (exn:fail:syntax? v)
   (string-contains?
    (exn-message v)
    "expected a function call, but there is no open parenthesis before this function")))
