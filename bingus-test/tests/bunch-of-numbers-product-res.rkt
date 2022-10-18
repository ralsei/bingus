(define (product bon)
  (cond [(none? bon) 1]
        [(some? bon) (* (some-first bon)
                        (product (some-rest bon)))]))
