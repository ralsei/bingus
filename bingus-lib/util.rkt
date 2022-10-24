#lang racket
(require racket/provide-syntax
         (for-syntax syntax/parse))
(provide TODO
         pascal->kebab
         structs-out
         set-add*

         current-resolved-system
         current-function-name
         current-function-type
         current-function-arguments)

(define (TODO . _) (error "unimplemented"))

(define (pascal->kebab str)
  (string-downcase
   (regexp-replace* #px"(?<!^)[A-Z]" str (λ (x) (string-append "-" x)))))

(define-provide-syntax (structs-out stx)
  (syntax-parse stx
    [({~literal structs-out} struct:id ...)
     #'(combine-out
        (struct-out struct)
        ...)]))

(define (set-add* st . vs)
  (for/fold ([curr-set st])
            ([v (in-list vs)])
    (set-add curr-set v)))

(define current-resolved-system (make-parameter (hash)))
(define current-function-name (make-parameter 'func))
(define current-function-type (make-parameter #f))
(define current-function-arguments (make-parameter #f))
