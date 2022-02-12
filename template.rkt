#lang racket
(require "ast.rkt"
         "util.rkt")
(provide resolve-system
         generate-template
         system->environment)

;; a system is a list of products, sums, and aliases
;; (read: top-level signature definitions)

(define (type-name ty)
  (cond [(product$? ty) (product$-name ty)]
        [(sum$? ty) (sum$-name ty)]
        [(alias$? ty) (alias$-name ty)]))

(define (system-lookup name system)
  (for/first ([ty (in-list system)]
              #:when (equal? (type-name ty) name))
    ty))

(define (resolve-system system)
  (define (resolve-alias name)
    (let loop ([current-val (alias$ name name)])
      (cond [(not (alias$? current-val)) current-val]
            [(not (string? (alias$-type current-val))) (alias$-type current-val)]
            [else (loop (system-lookup (alias$-type current-val) system))])))

  (for/hash ([ty (in-list system)])
    (values (type-name ty)
            (resolve-alias (type-name ty)))))

(define (generate-template name rsystem var-name)
  (define decl (hash-ref rsystem name))
  (cond [(product$? decl) (generate-product-template name rsystem var-name)]
        [(sum$? decl) (generate-sum-template name rsystem var-name)]
        [else var-name]))

(define (generate-product-template name rsystem var-name)
  (match-define (product$ (app pascal->kebab struct-name) fields) (hash-ref rsystem name))

  (app^ (hole^)
        (for/list ([fld (in-list fields)])
          (match-define (product-field$ accessor-name ty) fld)
          (define accessor (string->symbol (string-append struct-name "-" accessor-name)))
          (cond [(string? ty)
                 (generate-template ty rsystem
                                    (app^ accessor (list var-name)))]
                [else (app^ accessor (list var-name))]))))

(define (generate-sum-template name rsystem var-name)
  (define (generate-cond-question ty)
    (match ty
      [(singleton-atom$ val)
       (cond [(string? val) (app^ 'string=? (list var-name val))]
             [(number? val) (app^ '= (list var-name val))]
             [(boolean? val) (if val
                                 (app^ 'not (app^ 'false? (list var-name)))
                                 (app^ 'false? (list var-name)))]
             [else (error 'generate-sum-template
                          "invalid singleton value: ~a of signature ~a"
                          val ty)])]
      [_ (error 'generate-sum-template "currently unsupported: ~a" ty)]))

  (match-define (sum$ _ cases) (hash-ref rsystem name))
  (cond^
   (map (λ (x)
          (cond-case^ (generate-cond-question (sum-case$-type x)) (hole^)))
        cases)))

(module+ test
  (require rackunit)

  (define mad-lib-system
    (list
     (sum$ "Seeker"
           (list
            (sum-case$ (singleton-atom$ "finder"))
            (sum-case$ (singleton-atom$ "gadabout"))
            (sum-case$ (singleton-atom$ "hunter"))))
     (alias$ "Topspin" (string-atom$))
     (alias$ "Tokamak" (string-atom$))
     (product$ "FishtailPalm"
               (list
                (product-field$ "sapwood" "Seeker")
                (product-field$ "duramen" "Tokamak")
                (product-field$ "stump" "Topspin")))))

  (check-equal?
   (generate-template "Seeker" (resolve-system mad-lib-system) 's)
   (cond^
    (list (cond-case^ (app^ 'string=? '(s "finder")) (hole^))
          (cond-case^ (app^ 'string=? '(s "gadabout")) (hole^))
          (cond-case^ (app^ 'string=? '(s "hunter")) (hole^)))))
  ;; this shouldn't be called during synthesis, but here it is
  ;; (we should only be introducing templates for sums or products)
  (check-equal?
   (generate-template "Topspin" (resolve-system mad-lib-system) 's)
   's)
  (check-equal?
   (generate-template "FishtailPalm" (resolve-system mad-lib-system) 'ft)
   (app^ (hole^)
         (list
          (cond^
           (list (cond-case^
                  (app^ 'string=? (list (app^ 'fishtail-palm-sapwood '(ft)) "finder"))
                  (hole^))
                 (cond-case^
                  (app^ 'string=? (list (app^ 'fishtail-palm-sapwood '(ft)) "gadabout"))
                  (hole^))
                 (cond-case^
                  (app^ 'string=? (list (app^ 'fishtail-palm-sapwood '(ft)) "hunter"))
                  (hole^))))
          (app^ 'fishtail-palm-duramen '(ft))
          (app^ 'fishtail-palm-stump '(ft))))))

;; turns a system into an environment, by putting product accessors in scope
(define (system->environment system)
  (for/fold ([env (hash)])
            ([ty (in-list system)]
             #:when (product$? ty))
    (match-define (product$ struct-name fields) ty)
    (define kebab-name (pascal->kebab struct-name))

    (apply
     hash-set* env
     (string->symbol (string-append "make-" kebab-name))
     (function$ (map product-field$-type fields) struct-name)
     (string->symbol (string-append kebab-name "?"))
     (function$ (list (anything$)) (boolean-atom$))
     (for/fold ([xs '()])
               ([fld fields])
       (match-define (product-field$ field-name type) fld)
       (define accessor
         (string->symbol (string-append kebab-name "-" field-name)))
       (append (list accessor (function$ (list struct-name) type))
               xs)))))
