(define (product xs)
  (cond [(empty? xs) 1]
        [else (* (product (rest xs)) (first xs))]))
