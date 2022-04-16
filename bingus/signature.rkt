#lang racket
(provide (struct-out number-atom$)
         (struct-out string-atom$)
         (struct-out singleton-atom$)
         (struct-out product$)
         (struct-out product-field$)
         (struct-out sum$)
         (struct-out sum-case$)
         (struct-out alias$))

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
