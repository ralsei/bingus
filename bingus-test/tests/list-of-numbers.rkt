#lang htdp/bsl

;; A ListOfNumbers is one of:
;; - empty
;; - (cons Number ListOfNumbers)

;; product : ListOfNumbers -> Number
;; multiplies all the elements of the input list
(check-expect (product empty) 1)
(check-expect (product (list 1 2 3)) 6)
(check-expect (product (list 5 7 1)) 35)

(define (product xs)
  ...)

