#lang racket
(require "ast.rkt")
(provide init-bsl-environment)

(define init-bsl-environment
  (hash
   'not (function$ (list (boolean-atom$)) (boolean-atom$))
   ;'string=? (function$ (list (string-atom$) (string-atom$)) (boolean-atom$))
   ;'= (function$ (list (number-atom$) (number-atom$)) (boolean-atom$))
   'or (function$ (list (boolean-atom$) (boolean-atom$)) (boolean-atom$))
   '< (function$ (list (number-atom$) (number-atom$)) (boolean-atom$))
   '<= (function$ (list (number-atom$) (number-atom$)) (boolean-atom$))
   ;'* (function$ (list (number-atom$) (number-atom$)) (number-atom$))
   'add1 (function$ (list (number-atom$)) (number-atom$))
   ))
