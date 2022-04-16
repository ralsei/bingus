#lang racket
(require rackunit)
(provide permute-list inverse-permute-list
         permute-index inverse-permute-index
         inverse-permutation)

; A Permutation is one of:
; - #f
; - [ListOf NaturalNumber]
; Generally, the #:permute argument is a list of indices into our solution,
; and its inverse-permutation would be a list of indices into the submission.

; permute-list : Permutation [ListOf X] -> [ListOf X]
(define (permute-list perm lst)
  (if perm
    (for/list ([i perm]) (list-ref lst i))
    lst))
(check-equal? (permute-list '(1 2 0) '(a b c)) '(b c a))
(check-equal? (permute-list #f       '(a b c)) '(a b c))

; inverse-permute-list : Permutation [ListOf X] -> [ListOf X]
(define (inverse-permute-list perm lst)
  (if perm
    (map cdr (sort (map cons perm lst)
                   < #:key car))
    lst))
(check-equal? (inverse-permute-list '(1 2 0) '(a b c)) '(c a b))
(check-equal? (inverse-permute-list #f       '(a b c)) '(a b c))

; permute-index : Permutation NaturalNumber -> NaturalNumber
(define (permute-index perm index)
  (if perm (list-ref perm index) index))

; inverse-permute-index : Permutation NaturalNumber -> NaturalNumber
(define (inverse-permute-index perm index)
  (if perm (index-of perm index) index))

; inverse-permutation : Permutation -> Permutation
(define (inverse-permutation perm)
  (and perm (map cdr (sort (for/list ([i (in-list perm)] [j (in-naturals)])
                             (cons i j))
                           < #:key car))))
(check-equal? (inverse-permutation '(1 2 0)) '(2 0 1))
(check-equal? (inverse-permutation '(3 2 1 0)) '(3 2 1 0))
(check-equal? (inverse-permutation '(0 1 2 3)) '(0 1 2 3))
(check-equal? (inverse-permutation '(1 2 0 3)) '(2 0 1 3))
(check-equal? (inverse-permutation #f) #f)
