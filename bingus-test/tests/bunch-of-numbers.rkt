#lang htdp/bsl

;; A BunchOfNumbers is one of:
;; - (make-none)
;; - (make-some Number BunchOfNumbers)
(define-struct none [])
(define-struct some [first rest])

;; product : BunchOfNumbers -> Number
;; multiplies all the elements in the input
(check-expect (product (make-none)) 1)
(check-expect (product (make-some 1 (make-some 2 (make-some 3 (make-none))))) 6)
(check-expect (product (make-some 5 (make-some 7 (make-some 1 (make-none))))) 35)

(define (product bon)
  ...)

;; my-length : BunchOfNumbers -> Number
;; counts the number of elements in the given BunchOfNumbers
(check-expect (my-length (make-none)) 0)
(check-expect (my-length (make-some 1 (make-none))) 1)
(check-expect (my-length (make-some 1 (make-some 2 (make-none)))) 2)
(check-expect (my-length (make-some 1 (make-some 2 (make-some 3 (make-some 6 (make-some 9 (make-none)))))))
              5)

(define (my-length bon)
  ...)
