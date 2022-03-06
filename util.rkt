#lang racket
(require racket/provide-syntax
         (for-syntax syntax/parse))
(provide TODO
         pascal->kebab
         structs-out

         current-resolved-system
         current-function-name
         current-function-type)

(define (TODO) (error "unimplemented"))

(define (pascal->kebab str)
  (string-downcase
   (regexp-replace* #px"(?<!^)[A-Z]" str (λ (x) (string-append "-" x)))))

(define-provide-syntax (structs-out stx)
  (syntax-parse stx
    [({~literal structs-out} struct:id ...)
     #'(combine-out
        (struct-out struct)
        ...)]))

(define current-resolved-system (make-parameter (hash)))
(define current-function-name (make-parameter 'func))
(define current-function-type (make-parameter #f))
