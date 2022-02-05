#lang racket
(require racket/provide-syntax
         (for-syntax syntax/parse))
(provide TODO
         (struct-out fail)
         structs-out)

(define (TODO) (error "unimplemented"))
(struct fail () #:transparent)

(define-provide-syntax (structs-out stx)
  (syntax-parse stx
    [({~literal structs-out} struct:id ...)
     #'(combine-out
        (struct-out struct)
        ...)]))
