#lang htdp/bsl

;; A TrafficLight is one of:
;; - "red"
;; - "yellow"
;; - "green"

;; rotate : TrafficLight → TrafficLight
(define (rotate x) x)

(check-expect (rotate "red") "green")
(check-expect (rotate "yellow") "red")
(check-expect (rotate "green") "yellow")

;; synth
;; => introduce-lambda
;; (lambda (x) ...)
;; => EGUESS-VAR
;; x : TrafficLight
;; (lambda (x) x) => does not satisfy check-expects
;; => IREFINE-COND
;; (lambda (x)
;;   (cond [(string=? x "red") ...]
;;         [(string=? x "yellow") ...]
;;         [(string=? x "green") ...]))
;; => ... guess from checks...?

;; so I guess the next step is synthesizing the constant map
;; then we can refine from there
