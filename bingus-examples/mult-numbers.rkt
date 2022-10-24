;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname mult-numbers) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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