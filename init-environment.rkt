#lang racket
(require "ast.rkt")
(provide init-bsl-environment)

(define init-bsl-environment
  (hash
   'not (function$ (list (boolean-atom$)) (boolean-atom$))
   'string=? (function$ (list (string-atom$) (string-atom$)) (boolean-atom$))))
