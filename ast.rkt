#lang racket
(require zippers

         "util.rkt")
(provide
 ;;;; SIGNATURES
 (structs-out number-atom$
              string-atom$
              singleton-atom$
              function$
              product$
              product-field$
              sum$
              sum-case$
              alias$
              no-goal$)
 ;;;; SYNTAX
 (structs-out lambda^
              hole^)
 (struct-zipper-out lambda^
                    hole^)
 ;;;; CHECKS
 (struct-out check^))

;;;; SIGNATURES
;; NOTE:
;; - we probably want a better way to think about recursion in the AST
;;   (but recursion comes later)

;; atomic datatypes
;; one of: Number, String
(struct number-atom$ () #:transparent)
(struct string-atom$ () #:transparent)

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

;; no goal (for zippers)
(struct no-goal$ () #:transparent)

;;;; SYNTAX
;; (lambda (x y) x)
;; => (lambda^ (list 'x 'y) 'x)
(struct lambda^ (formals body) #:transparent)

;; holes
(struct hole^ () #:transparent)

;; zipper frames
(define-struct-zipper-frames lambda^ hole^)

;;;; CHECKS
;; (check-expect (add1 5) 6)
;; => (check^ '(add1 5) 6)
(struct check^ (actual expected) #:transparent)
