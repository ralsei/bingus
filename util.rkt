#lang racket
(provide TODO
         (struct-out fail))

(define (TODO) (error "unimplemented"))
(struct fail () #:transparent)
