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
    (define start-pos (send defs get-start-position))
    (define end-pos (send defs get-forward-sexp start-pos))
    (define synth-expr (read (open-input-string (send defs get-text start-pos end-pos))))
    
    (define-values (fn args) (definition-name synth-expr))

    (define fr (new frame% [label "Synthesizing..."]))
    (void (new message%
               [parent fr]
               [label (format "Generating code for: ~a" fn)]))
    (send fr center)
    (send fr show #t)

    (define res (synthesize defs-port fn args))

    (send fr show #f)

    (send defs begin-edit-sequence)
    (send defs remove-sexp start-pos)
    (send defs insert (pretty-format res #:mode 'write) start-pos)
    (send defs end-edit-sequence)

    #f))
