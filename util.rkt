#lang racket
(require racket/provide-syntax
         (for-syntax syntax/parse))
(provide TODO
         (struct-out fail)
         pascal->kebab
         structs-out)

(define (TODO) (error "unimplemented"))
(struct fail () #:transparent)

(define (pascal->kebab str)
  (string-downcase
   (regexp-replace* #px"(?<!^)[A-Z]" str (λ (x) (string-append "-" x)))))

(define-provide-syntax (structs-out stx)
  (syntax-parse stx
    [({~literal structs-out} struct:id ...)
     #'(combine-out
        (struct-out struct)
        ...)]))
