#lang htdp/bsl

;; A Point is a (make-point Number Number)
(define-struct point [x y])

;; A EvenMorePoints is one of:
;; - (make-none)
;; - (make-one Point)
;; - (make-two Point Point)
;; - (make-three Point Point Point)
(define-struct none [])
(define-struct one [first])
(define-struct two [first second])
(define-struct three [first second third])

;; add-point : Point EvenMorePoints -> EvenMorePoints
;; Adds a point to the given EvenMorePoints, discarding the third if we run
;; out of space.
(check-expect (add-point (make-point 3 2) (make-none))
              (make-one (make-point 3 2)))
(check-expect (add-point (make-point 3 2)
                         (make-one (make-point 4 5)))
              (make-two (make-point 3 2)
                        (make-point 4 5)))
(check-expect (add-point (make-point 9 2)
                         (make-two (make-point 9 3)
                                   (make-point 8 7)))
              (make-three (make-point 9 2)
                          (make-point 9 3)
                          (make-point 8 7)))
(check-expect (add-point (make-point 10 10)
                         (make-three (make-point 9 2)
                                     (make-point 9 3)
                                     (make-point 8 7)))
              (make-three (make-point 10 10)
                          (make-point 9 2)
                          (make-point 9 3)))

(define (add-point p emp)
  ...)
