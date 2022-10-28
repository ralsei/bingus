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

;; add-ns : Number ListOfNumbers -> ListOfNumbers
;; adds n to each element of the input list
(check-expect (add-ns 3 empty) empty)
(check-expect (add-ns 4 (list 1 2 3))
              (list 5 6 7))
(check-expect (add-ns 9 (list 3 27 2))
              (list (+ 3 9) (+ 27 9) (+ 9 2)))

(define (add-ns n xs)
  ...)
