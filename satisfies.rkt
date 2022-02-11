#lang racket
(require racket/sandbox
         "ast.rkt")
(provide satisfies?)

;; we use racket/base largely for speed -- otherwise, we would have to recreate the evaluator
;; every time, since *SL doesn't allow redefinitions
;;
;; plus, the η-contraction phase (if there is one) will mean we can check before eta-contraction here
;; since racket supports it but BSL doesn't
(define base-eval
  (make-evaluator 'racket/base))

(define (satisfies? prog checks)
  (base-eval prog)
  (for/and ([check (in-list checks)])
    (match-define (check^ actual expected) check)
    (equal? (base-eval actual) (base-eval expected))))