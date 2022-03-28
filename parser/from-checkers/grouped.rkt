#lang racket
(require rackunit)
(provide grouped-string-split)

; group : [ListOf String] -> [ListOf String]
; Try to concatenate every subsequence of `strings` that begins with some car
; in `delims`, ends with the corresponding cdr, and does not otherwise contain
; any string in `delims`
(define (group strings)
  (define delims '(("(" . ")") ("[" . "]")))
  (cond [(null? strings) strings]
        [(assoc (car strings) delims)
         => (lambda (delim) (let accum ([rest (cdr strings)]
                                        [seen (list (car strings))])
            ; *Accumulator*: `seen` is the reverse list of strings that
            ; does not contain any string in `delims`, since the most recent
            ; (car delim) inclusive
            (cond [(null? rest)
                   ; (car delim) unmatched
                   (reverse seen)]
                  [(string=? (car rest) (cdr delim))
                   ; (car delim) matched (cdr delim)
                   (cons (foldl string-append (car rest) seen)
                         (group (cdr rest)))]
                  [(for/or ([d delims]) (or (string=? (car rest) (car d))
                                            (string=? (car rest) (cdr d))))
                   ; group broken by some string in `delims`
                   (foldl cons (group rest) seen)]
                  [else (accum (cdr rest) (cons (car rest) seen))])))]
        [else (cons (car strings) (group (cdr strings)))]))
(check-equal? (group (list " " "(" "a" "(" "b" ")" ")" ")" " "))
              (list " " "(" "a" "(b)" ")" ")" " "))
(check-equal? (group (list " " "(" "a" "(b)" ")" ")" " "))
              (list " " "(a(b))" ")" " "))
(check-equal? (group (list " " "(a(b))" ")" " "))
              (list " " "(a(b))" ")" " "))

; grouped-string-split : String -> [ListOf String]
; Do our best to interpret a string as a list of things that might include
; nested parenthesized, bracketed, or double-quoted parts. Also arrows-->
;(check-equal? (grouped-string-split " apple  banana \t  cherry\n\r")
;              (list "apple" "banana" "cherry"))
(define (grouped-string-split s)
  (define parts1
    (regexp-match* #px"\\\"(?:\\\\.|[^\\\"])*\\\"|[^][(){}\",'`|;#\\s]+|." s))
  (define parts2
    (for*/list ([part1 parts1]
                [part2 (if (string-prefix? part1 "\"")
                         (in-value part1)
                         (in-list (regexp-match* #px"-+>" part1 #:gap-select? #t)))])
      part2))
  (let loop ([strings parts2])
    (let ([grouped (group strings)])
      (if (> (length strings) (length grouped))
        (loop grouped)
        (filter (lambda (s) (regexp-match? #px"\\S" s)) grouped)))))
(check-equal? (grouped-string-split " (a(b))) foo ")
              (list "(a(b))" ")" "foo"))
(check-equal? (grouped-string-split " (a[b)]) foo ")
              (list "(" "a" "[" "b" ")" "]" ")" "foo"))
(check-equal? (grouped-string-split "--->[A->B][[A->B]->C]")
              (list "--->" "[A->B]" "[[A->B]->C]"))
