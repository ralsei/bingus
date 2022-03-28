#lang racket
(require zippers

         "util.rkt")
(provide
 ;;;; SIGNATURES
 (structs-out anything$
              number-atom$
              string-atom$
              boolean-atom$
              singleton-atom$
              function$
              product$
              product-field$
              sum$
              sum-case$
              defn$
              recur$)
 ;;;; SYNTAX
 (structs-out lambda^
              app^
              cond^
              cond-case^
              hole^)
 (struct-zipper-out lambda^
                    app^
                    cond^
                    cond-case^)
 ;;;; SYNTAX HELPERS
 plug/ast
 first-hole/ast
 next-hole/ast
 complete?
 ;;;; CHECKS
 (struct-out check^))

;;;; SIGNATURES
;; NOTE:
;; - we probably want a better way to think about recursion in the AST
;;   (but recursion comes later)

;; anything
(struct anything$ () #:transparent)

;; atomic datatypes
;; one of: Number, String, Boolean
(struct number-atom$ () #:transparent)
(struct string-atom$ () #:transparent)
(struct boolean-atom$ () #:transparent)

;; single values
;; not actually used alone (generally), usually used as part of enums
;; parser should account for that
(struct singleton-atom$ (value) #:transparent)

;; functions
;; Number String -> Number
;; =>
;; (function$ (list (number-atom$) (string-atom$)) (number-atom$))
(struct function$ (inputs output) #:transparent)

;; structures/products
(struct product$ (name fields) #:transparent)
(struct product-field$ (name type) #:transparent)

;; enumerations/sums
;; A TrafficLight is one of:
;; - "red"
;; - "yellow"
;; - "green"
;; =>
;; (sum$
;;  (list
;;   (sum-case$ (singleton-atom$ "red"))
;;   (sum-case$ (singleton-atom$ "yellow"))
;;   (sum-case$ (singleton-atom$ "green"))))
(struct sum$ (cases) #:transparent)
(struct sum-case$ (type) #:transparent)

;; definitions -- top-level signatures
(struct defn$ (name type) #:transparent)

;; marks recursion in a resolved system
;; TODO: mutual rcursion -- hence why the `on` field is here
;;       right now it's just set to the name of the type and is unused
(struct recur$ (on) #:transparent)

;;;; SYNTAX
;; variables are just symbols
;; numbers are just numbers
;; strings are just strings
;;
;; this might be changed, since BSL has symbols
;; (but we don't encourage using them in 211, at least)

;; (lambda (x y) x)
;; => (lambda^ (list 'x 'y) 'x)
(struct lambda^ (formals body) #:transparent)

;; (string=? c "red")
;; =>
;; (app^ 'string=? (list 'c "red"))
(struct app^ (rator rand) #:transparent)

;; (cond [(string=? c "red") "blue"]
;;       [(string=? c "blue") "red"])
;; =>
;; (cond^
;;  (list
;;   (cond-case^ (app 'string=? (list 'c "red")) "blue")
;;   (cond-case^ (app 'string=? (list 'c "blue")) "red")))
(struct cond^ (clauses) #:transparent)
(struct cond-case^ (question answer) #:transparent)

;; holes
(struct hole^ (can-fill-const? cenv signature checks) #:transparent)

;; zipper frames
;; we explicitly don't define one for a hole, since you shouldn't be able
;; to zip into one
(define-struct-zipper-frames lambda^ app^ cond^ cond-case^)

;; utility functions (derived movements)
;; go all the way left through the ast
(define (first/ast exp)
  (match-define (zipper focus _) exp)
  (cond [(hole^? focus) exp]
        [(lambda^? focus) (first/ast (down/lambda^-body exp))]
        [(list? focus)
         (cond [(empty? focus) exp]
               [else (first/ast (down/list-first exp))])]
        [(app^? focus) (first/ast (down/app^-rand exp))]
        [(cond^? focus) (first/ast (down/cond^-clauses exp))]
        [(cond-case^? focus) (first/ast (down/cond-case^-answer exp))]
        ;; numbers, symbols, etc
        [else exp]))

;; goes to the next ancestor
(define (next-ancestor/ast exp)
  (cond [(zipper-at-top? exp) exp]
        [else
         (match-define (zipper focus (cons first-ctx _)) exp)
         (cond
           [(list-item-frame? first-ctx)
            (cond [(empty? (list-item-frame-to-right first-ctx)) (up exp)]
                  [else (right/list exp)])]
           [else (next-ancestor/ast (up exp))])]))

(define (next/ast exp)
  (first/ast (next-ancestor/ast exp)))

(define (first-hole/ast exp)
  (match-define (and result (zipper focus _)) (first/ast exp))
  (cond [(equal? result exp) result]
        [(zipper-at-top? result) result]
        [(hole^? focus) result]
        [else (first-hole/ast (next/ast exp))]))

(define (next-hole/ast exp)
  (match-define (and result (zipper focus _)) (next/ast exp))
  (cond [(zipper-at-top? exp) exp]
        [(hole^? focus) result]
        [else (next-hole/ast (up exp))]))

;; fill the given hole, and go to the next one
(define (plug/ast fill exp)
  (cond [(hole^? (zipper-focus exp)) (edit (const fill) exp)]
        [else (error 'plug/ast "not focused on a hole: ~a" exp)]))

(define (complete? unzipped-exp)
  (match unzipped-exp
    [y #:when (or (symbol? y)
                  (number? y)
                  (string? y)
                  (boolean? y))
       #t]
    [(lambda^ _ body) (complete? body)]
    [(app^ _ rand) (complete? rand)]
    [(cond^ clauses) (andmap complete? clauses)]
    [(cond-case^ question answer) (and (complete? question)
                                       (complete? answer))]
    [(? list?) (andmap complete? unzipped-exp)]
    [(hole^ _ _ _ _) #f]
    [_ (error 'complete? "unsupported form: ~a" unzipped-exp)]))

;;;; CHECKS
;; (check-expect (add1 5) 6)
;; => (check^ '(add1 5) 6)
(struct check^ (actual expected) #:transparent)
