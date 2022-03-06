#lang htdp/bsl

; A BunchOfNumbers is one of:
; - (make-none)
; - (make-some Number BunchOfNumbers)
(define-struct none [])
(define-struct some [first rest])

; product : BunchOfNumbers -> Number
; multiplies all elements in a BunchOfNumbers
(define (product bon)
  ...)

(check-expect (product (make-none)) 1)
(check-expect (product (make-some 1 (make-some 2 (make-some 3 (make-none)))))
              6)
(check-expect (product (make-some 5 (make-some 7 (make-some 1 (make-none)))))
              35)
