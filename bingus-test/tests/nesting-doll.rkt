#lang htdp/bsl

;; A Doll is one of:
;; - (make-small-doll String)
;; - (make-larger-doll Doll)
(define-struct small-doll (color))
(define-struct larger-doll (smaller))

;; green-doll : Doll -> Doll
;; takes a doll and returns a similar doll, but the smallest doll is green
(check-expect (green-doll (make-small-doll "red")) (make-small-doll "green"))
(check-expect (green-doll (make-larger-doll (make-small-doll "green")))             
              (make-larger-doll (make-small-doll "green")))
(check-expect (green-doll (make-larger-doll (make-larger-doll (make-small-doll "purple"))))
              (make-larger-doll (make-larger-doll (make-small-doll "green"))))

(define (green-doll d)
  ...)
