#lang racket/base
(require racket/class
         racket/gui
         racket/match
         racket/pretty
         bingus
         quickscript)

(define (definition-name expr)
  (match expr
    [`(define (,n ,args ...) ,_ ...) (values n args)]
    [_ (error "selection isn't a well-formed function definition: " expr)]))

(script-help-string "Calls the Bingus program synthesizer on the selection.")

(define-script bingus
  #:label "Bingus"
  (λ (selection
      #:definitions defs)
    (define defs-port (open-input-string (send defs get-flattened-text)))
    (define sel-expr (read (open-input-string selection)))
    
    (define-values (fn args) (definition-name sel-expr))

    (pretty-format
     (synthesize defs-port fn args)
     #:mode 'write)))
