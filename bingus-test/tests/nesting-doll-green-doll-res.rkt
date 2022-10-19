(define (green-doll d)
  (cond [(small-doll? d) (make-small-doll "green")]
        [(larger-doll? d) (make-larger-doll (green-doll (larger-doll-smaller d)))]))
