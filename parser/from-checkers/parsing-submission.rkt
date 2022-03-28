#lang racket
(require "data-def-parsing.rkt"
         "check-signature.rkt")
(provide parse-submission)

; mode controls what is parsed
;  - 'a is for all lines
;  - 'c is for all comments
;  - 'd is for all data definitions
;  - 's is for all signatures

(define (parse submitted-string [mode 'a])
  (let ([regexp-exp
         (match mode
           ['a #px"(?m:^.*$)"]
           ['c #px"(?m:^\\s*;.*$)"]
           ['d #px"(?mi:^\\s*;[\\s;]*an?\\s+(.*\\S)\\s+is\\s+(?:an?\\s+)?(.*\\S)\\s*$)"]
           ['s #px"(?m:^\\s*;[\\s;]*([^][(){}\",'`|;#\\s]+)\\s*:\\s*(.*?)\\s*--*>\\s*(.*?)\\s*$)"]
           )])
    (regexp-match* regexp-exp submitted-string)))

(define (parse-submission ss)
  (let ([signatures (parse ss 's)]
        [dd (parse ss 'd)])
    (values (add-all-dd dd '())
            (map ss->signature signatures))))
    
