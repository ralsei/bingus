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
              alias$)
 ;;;; SYNTAX
 (structs-out lambda^
              app^
              cond^
              cond-case^
              hole^)
 (struct-zipper-out lambda^
                    app^
                    cond^
                    cond-case^
                    hole^)
 ;;;; SYNTAX HELPERS
 plug/ast
 first-hole/ast
 next-hole/ast
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
;; A Position is a (make-position Number Number)
;; (struct position (x y))
;; =>
;; (product$
;;  "Position"
;;  (list
;;   (product-field$ "x" (number-atom$))
;;   (product-field$ "y" (number-atom$))))
(struct product$ (name fields) #:transparent)
(struct product-field$ (name type) #:transparent)

;; enumerations/sums
;; A TrafficLight is one of:
;; - "red"
;; - "yellow"
;; - "green"
;; =>
;; (sum$
;;  "TrafficLight"
;;  (list
;;   (sum-case$ (singleton-atom$ "red"))
;;   (sum-case$ (singleton-atom$ "yellow"))
;;   (sum-case$ (singleton-atom$ "green"))))
(struct sum$ (name cases) #:transparent)
(struct sum-case$ (type) #:transparent)

;; type alias
;; A Time is a Number
;; =>
;; (alias$ "Time" (number-atom$))
(struct alias$ (name type) #:transparent)

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
(struct hole^ (can-fill-const?) #:transparent)

;; zipper frames
(define-struct-zipper-frames lambda^ app^ cond^ cond-case^ hole^)

;; utility functions (derived movements)
;; go all the way left through the ast
(define (first/ast exp)
  (match-define (zipper focus _) exp)
  (cond [(hole^? focus) exp]
        [(lambda^? focus) (first/ast (down/lambda^-body exp))]
        [(list? focus) (first/ast (down/list-first exp))]
        [(app^? focus) (first/ast (down/app^-rand exp))]
        [(cond^? focus) (first/ast (down/cond^-clauses exp))]
        [(cond-case^? focus) (first/ast (down/cond-case^-answer exp))]
        ;; numbers, symbols, etc
        [else exp]))

;; goes to the next ancestor
(define (next-ancestor/ast exp)
  (match-define (zipper focus (cons first-ctx _)) exp)
  (cond [(zipper-at-top? exp) exp]
        [(list-item-frame? first-ctx)
         (cond [(empty? (list-item-frame-to-right first-ctx)) (up exp)]
               [else (right/list exp)])]
        [else (next-ancestor/ast (up exp))]))

(define (next/ast exp)
  (first/ast (next-ancestor/ast exp)))

(define (first-hole/ast exp)
  (match-define (and result (zipper focus _)) (first/ast exp))
  (cond [(hole^? focus) result]
        [else (first-hole/ast (next/ast exp))]))

(define (next-hole/ast exp)
  (match-define (and result (zipper focus _)) (next/ast exp))
  (cond [(hole^? focus) result]
        [else (next-hole/ast (up exp))]))

;; fill the given hole, and go to the next one
(define (plug/ast fill exp)
  (cond [(hole^? (zipper-focus exp)) (edit (const fill) exp)]
        [else (error 'plug/ast "not focused on a hole: ~a" exp)]))

(module+ test
  (require rackunit)

  (define cond-2-holes
    (zip
     (cond^
      (list
       (cond-case^ (app^ 'not (list (app^ 'false? (list 'x))))
                   (hole^ #t))
       (cond-case^ (app^ 'false? (list 'x))
                   (hole^ #t))))))
  (check-equal?
   (first-hole/ast cond-2-holes)
   (zipper (hole^ #t)
           (list (cond-case^-answer-frame (app^ 'not (list (app^ 'false? '(x)))))
                 (list-item-frame '() (list (cond-case^ (app^ 'false? '(x)) (hole^ #t))))
                 (cond^-clauses-frame))))
  (check-equal?
   (next-hole/ast (first-hole/ast cond-2-holes))
   (zipper (hole^ #t)
           (list (cond-case^-answer-frame (app^ 'false? '(x)))
                 (list-item-frame (list (cond-case^ (app^ 'not (list (app^ 'false? '(x)))) (hole^ #t))) '())
                 (cond^-clauses-frame))))

  (define app-2-holes
    (zip (app^ 'string=? (list (hole^ #t) (hole^ #t)))))
  (define hole-fill
    (app^ 'string-append (list (hole^ #t) "hi")))
  (check-equal?
   (first-hole/ast app-2-holes)
   (zipper (hole^ #t)
           (list (list-item-frame '() (list (hole^ #t)))
                 (app^-rand-frame 'string=?))))
  (check-equal?
   (plug/ast hole-fill (first-hole/ast app-2-holes))
   (zipper (app^ 'string-append (list (hole^ #t) "hi"))
           (list (list-item-frame '() (list (hole^ #t)))
                 (app^-rand-frame 'string=?))))
  (check-equal?
   (first-hole/ast (plug/ast hole-fill (first-hole/ast app-2-holes)))
   (zipper (hole^ #t)
           (list (list-item-frame '() '("hi"))
                 (app^-rand-frame 'string-append)
                 (list-item-frame '() (list (hole^ #t)))
                 (app^-rand-frame 'string=?))))
  (check-equal?
   (plug/ast 'x (first-hole/ast (plug/ast hole-fill (first-hole/ast app-2-holes))))
   (zipper 'x
           (list (list-item-frame '() '("hi"))
                 (app^-rand-frame 'string-append)
                 (list-item-frame '() (list (hole^ #t)))
                 (app^-rand-frame 'string=?))))
  (check-equal?
   (next-hole/ast (plug/ast 'x (first-hole/ast (plug/ast hole-fill (first-hole/ast app-2-holes)))))
   (zipper (hole^ #t)
           (list (list-item-frame (list (app^ 'string-append '(x "hi"))) '())
                 (app^-rand-frame 'string=?)))))

;;;; CHECKS
;; (check-expect (add1 5) 6)
;; => (check^ '(add1 5) 6)
(struct check^ (actual expected) #:transparent)
