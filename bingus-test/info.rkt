#lang info

(define collection "bingus-test")

(define test-omit-paths '("./info.rkt"))
(define test-responsibles '((all hazel@knightsofthelambdacalcul.us)))

(define pkg-desc "Tests for Bingus")
(define version "0.0001")
(define deps '("base"
               "rackunit-lib"
               "bingus-lib"))
