#lang racket/base
(require racket/class
         racket/match
         racket/pretty
         bingus
         quickscript)

(script-help-string "Calls the Bingus program synthesizer on the selection.")

(define (definition-name expr)
  (match expr
    [`(define (,n ,_ ...) ,_ ...) n]
    [_ (error "selection isn't a well-formed function definition")]))

(define-script bingus
  #:label "Bingus"
  (λ (selection
      #:definitions defs)
    (define defs-port (open-input-string (send defs get-flattened-text)))

    (define sel-expr (read (open-input-string selection)))

    (pretty-format
     (synthesize defs-port (definition-name sel-expr))
     #:mode 'write)))
