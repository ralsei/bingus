;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname mult-numbers.synth) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A TreeOfNumbers is one of:
; - (make-leaf Number)
; - (make-node TreeOfNumbers TreeOfNumbers)
(define-struct leaf [n])
(define-struct node [left right])

; prod-tree : TreeOfNumbers -> Number
; multiplies all elements in a TreeOfNumbers
(define (prod-tree ton)
  ...)

(check-expect (prod-tree (make-leaf 3)) 3)
(check-expect (prod-tree (make-node (make-leaf 3) (make-leaf 9)))
              27)
(check-expect (prod-tree (make-node (make-node (make-leaf 3) (make-leaf 9))
                                    (make-leaf 3)))
              81)
