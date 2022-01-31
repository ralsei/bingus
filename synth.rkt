#lang racket
(require
 racket/hash

 "ast.rkt"
 "bsl-require.rkt"
 "unparse.rkt"
 "util.rkt")

;; compile-time environment is a hash table of terms to types

;; try each of the following functions
;; if any of them synthesize, check that it satisfies
;; if it satisfies, return
(define (try-each ty checks env . fns)
  (or
   (for*/or ([fn (in-list fns)]
             [result (in-value (fn ty checks env))]
             #:when (not (fail? result))
             [prog (in-value (unparse result))]
             #:when
             (begin
               (displayln prog)
               (satisfies? prog checks)))
     prog)
   (fail)))

(define (satisfies? prog checks)
  (for/and ([check (in-list checks)])
    (match-define (check^ actual expected) check)
    (equal? (eval/bsl prog actual)
            (eval/bsl prog expected))))

(define (synth-and-check ty checks [env (hash)])
  (define res (synth ty checks env))
  (and (not (fail? res))
       (satisfies? (list (unparse res)) checks)
       res))

(define (synth ty checks env)
  (match ty
    [(function$ ins out) (introduce-lambda ins out checks env)]
    [_ (eguess ty checks env)]))

;; if we have a function type, introduce binders
;; function$ → lambda^
;;
;; (this is probably what the Myth paper means by η-long form,
;;  since we always do this)
(define (introduce-lambda ins out checks env)
  (define args (map (λ (_) (gensym)) ins))
  (define new-env
    (hash-union (for/hash ([arg (in-list args)]
                           [ty (in-list ins)])
                  (values arg ty))
                env))
  (lambda^ args (synth out checks new-env)))

;;;; EGUESS
(define (eguess ty checks env)
  (eguess-var ty checks env))

;; EGUESS_VAR
;; x : τ ∈ Γ
;; -------------
;; Σ; Γ ⊢ τ E→ x
;;
;; run through the environment guessing anything in it that might match
;; the signature
;; if we find one, try and see if it would satisfy the spec
;; if not, keep going
(define (eguess-var ty checks env)
  (or
   (for/first ([(t τ) (in-hash env)]
               #:when (equal? τ ty))
     t)
   (fail)))
