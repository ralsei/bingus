#lang racket
(require "ast.rkt")
(provide init-bsl-environment)

(define init-bsl-environment
  (hash 'add1 (function$ (list (number-atom$)) (number-atom$) #f)
        '+ (function$ (list (number-atom$) (number-atom$)) (number-atom$) #f)
        '* (function$ (list (number-atom$) (number-atom$)) (number-atom$) #f)
        '- (function$ (list (number-atom$)) (number-atom$) #f)
        ))
