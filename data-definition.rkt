#lang racket
(require racket/hash
         racket/syntax

         "ast.rkt"
         "util.rkt")
(provide resolve-system
         generate-sum-template
         generate-product-environment
         system->environment)

(define (system-ref name system)
  (for/first ([defn (in-list system)]
              #:when (equal? (defn$-name defn) name))
    (defn$-type defn)))

;; a system is a list of defn$ses,
;; which correspond to top-level definitions
;;
;; a resolved system is a hash of strings to signatures,
;; with no definitions, and no indirection
;;
;; recursion is replaced with a marker, which signifies the type
;; to recur on
;; NOTE: right now that's just the single type, since we don't
;;       synthesize helpers
(define (resolve-system system)
  (define ((insert-recursion name) sig)
    (match sig
      [(? string?) (cond [(equal? sig name) (recur$ sig)]
                         [else (resolve-defn sig)])]
      [(product$ n fields) (product$ n (map (insert-recursion name) fields))]
      [(product-field$ n type) (product-field$ n ((insert-recursion name) type))]
      [(sum$ cases) (sum$ (map (insert-recursion name) cases))]
      [(sum-case$ ty) (sum-case$ ((insert-recursion name) ty))]
      ;; TODO: look into this more. this is very wrong lol
      [y y]))

  (define (resolve-defn name)
    (let loop ([current-val name])
      (cond [(not (string? current-val)) ((insert-recursion name) current-val)]
            [else (loop (system-ref name system))])))

  (for/hash ([decl (in-list system)])
    (values (defn$-name decl)
            (resolve-defn (defn$-name decl)))))

;; turn each product field into an entry in the environment,
;; UNLESS it's recursive, in which case insert recursion here
(define (generate-product-environment ty var-name)
  (match-define (product$ struct-name fields) ty)

  (for/fold ([cenv (hash)])
            ([fld (in-list fields)])
    (match-define (product-field$ accessor-name out) fld)
    (define accessor
      (format-symbol "~a-~a" struct-name accessor-name))

    ;; TODO: this only works for unary recursion
    ;; (so like, sum-numbers)
    ;; the fix for this is to pass in the function signature and then do natural recursion
    ;;
    ;; also the function name is hardcoded, which is not great
    (match out
      [(recur$ on)
       (hash-set* cenv
                  (app^ accessor (list var-name)) on
                  (app^ 'func (app^ accessor (list var-name))) on)]
      [_ (hash-set cenv (app^ accessor (list var-name)) out)])))

(define (generate-sum-template name rsystem
                               #:var-name var-name
                               #:cenv cenv
                               #:signature sig
                               #:checks checks)
  (define (generate-cond-clause ty)
    (match ty
      [(singleton-atom$ val)
       (cond-case^
        (cond [(string? val) (app^ 'string=? (list var-name val))]
              [(number? val) (app^ '= (list var-name val))]
              [(boolean? val) (if val
                                  (app^ 'not (app^ 'false? (list var-name)))
                                  (app^ 'false? (list var-name)))]
              [else (error 'generate-sum-template
                           "invalid singleton value: ~a of signature ~a"
                           val ty)])
        (hole^ #t cenv sig checks))]
      [(product$ name _)
       (cond-case^
        (app^ (string->symbol (string-append name "?")) (list var-name))
        (hole^ #t
               (hash-union cenv (generate-product-environment ty var-name))
               sig
               ;; TODO: narrow checks here
               checks))]
      [_ (error 'generate-sum-template "currently unsupported: ~a" ty)]))

  (match-define (sum$ cases) (hash-ref rsystem name))
  (cond^
   (map (λ (x)
          (generate-cond-clause (sum-case$-type x)))
        cases)))

;; turns a system into an environment, by putting product accessors in scope
;; TODO: this has to be recursive now, b/c unions
(define (system->environment system)
  (for/fold ([env (hash)])
            ([defn (in-list system)]
             #:when (product$? (defn$-type defn)))
    (match-define (product$ struct-name fields) (defn$-type defn))

    (hash-set* env
               (format-symbol "make-~a" struct-name)
               (function$ (map product-field$-type fields) (defn$-name defn))
     ;; commented out for now, as guessing something? isn't useful
     #;(string->symbol (string-append struct-name "?"))
     #;(function$ (list (anything$)) (boolean-atom$)))))

(module+ test
  (require rackunit)

  (define mad-lib-system
    (list
     (defn$ "Seeker"
       (sum$ (list
              (sum-case$ (singleton-atom$ "finder"))
              (sum-case$ (singleton-atom$ "gadabout"))
              (sum-case$ (singleton-atom$ "hunter")))))
     (defn$ "Topspin" (string-atom$))
     (defn$ "Tokamak" (string-atom$))
     (defn$ "FishtailPalm"
       (product$ "fishtail-palm"
                 (list
                  (product-field$ "sapwood" "Seeker")
                  (product-field$ "duramen" "Tokamak")
                  (product-field$ "stump" "Topspin"))))))

  (check-equal?
   (resolve-system mad-lib-system)
   (hash
    "FishtailPalm" (product$
                    "fishtail-palm"
                    (list
                     (product-field$
                      "sapwood"
                      (sum$
                       (list
                        (sum-case$ (singleton-atom$ "finder"))
                        (sum-case$ (singleton-atom$ "gadabout"))
                        (sum-case$ (singleton-atom$ "hunter")))))
                     (product-field$ "duramen" (string-atom$))
                     (product-field$ "stump" (string-atom$))))
    "Seeker" (sum$
              (list
               (sum-case$ (singleton-atom$ "finder"))
               (sum-case$ (singleton-atom$ "gadabout"))
               (sum-case$ (singleton-atom$ "hunter"))))
    "Tokamak" (string-atom$)
    "Topspin" (string-atom$)))

  (check-equal?
   (generate-sum-template "Seeker" (resolve-system mad-lib-system)
                          #:var-name 's
                          #:cenv (hash)
                          #:signature (number-atom$)
                          #:checks '())
   (cond^
    (list
     (cond-case^ (app^ 'string=? '(s "finder")) (hole^ #t '#hash() (number-atom$) '()))
     (cond-case^ (app^ 'string=? '(s "gadabout")) (hole^ #t '#hash() (number-atom$) '()))
     (cond-case^ (app^ 'string=? '(s "hunter")) (hole^ #t '#hash() (number-atom$) '())))))

  (define bon-system
    (list
     (defn$ "BunchOfNumbers"
       (sum$ (list (sum-case$ (product$ "none" '()))
                   (sum-case$ (product$ "some"
                                        (list
                                         (product-field$ "first" (number-atom$))
                                         (product-field$ "rest" "BunchOfNumbers")))))))))

  (check-equal?
   (resolve-system bon-system)
   (hash
    "BunchOfNumbers"
    (sum$
     (list
      (sum-case$ (product$ "none" '()))
      (sum-case$
       (product$ "some" (list (product-field$ "first" (number-atom$))
                              (product-field$ "rest" (recur$ "BunchOfNumbers")))))))))

  (check-equal?
   (generate-sum-template "BunchOfNumbers" (resolve-system bon-system)
                          #:var-name 'bon
                          #:cenv (hash)
                          #:signature (number-atom$)
                          #:checks '())
   (cond^
    (list
     (cond-case^ (app^ 'none? '(bon)) (hole^ #t '#hash() (number-atom$) '()))
     (cond-case^ (app^ 'some? '(bon))
                 (hole^
                  #t
                  (hash (app^ 'some-first '(bon)) (number-atom$)
                        (app^ 'func (app^ 'some-rest '(bon))) "BunchOfNumbers"
                        (app^ 'some-rest '(bon)) "BunchOfNumbers")
                  (number-atom$)
                  '()))))))
