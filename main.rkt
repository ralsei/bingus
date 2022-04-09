#lang racket
(require "parser/parse.rkt"
         "parser/from-checkers/datadef.rkt"
         "synth.rkt")

(define (fresh-eval x) (eval x (make-base-namespace)))

(define debug-mode (make-parameter #f))

(define (parse-command-line-args)
  (command-line
   #:program "bingus"
   #:once-each
   [("-v" "--verbose") "Print partial programs with holes"
                       (debug-mode #t)]
   #:args (filename procname)
   (values filename (string->symbol procname))))

;; Path Symbol -> Program
(define (synthesize filename proc-to-synthesize)
  (define lines (file->lines filename))
  (define sexps (read-file-with-lang filename))

  ; grab the datadefs
  (define system (checkers-dds->bingus-system
                  (parse-datadefs lines fresh-eval #:built-in '())
                  sexps))
  ; the signature
  (define sig (checkers-polysigs->bingus-signature
               (parse-polysigs lines fresh-eval)
               proc-to-synthesize))
  ; and the checks
  (define checks (parse-checks sexps proc-to-synthesize))
  ; let 'er rip
  (run-synth proc-to-synthesize sig system checks #:debug? (debug-mode)))

(module+ main
  (define-values (filename procname) (parse-command-line-args))
  (pretty-write (synthesize filename procname)))
