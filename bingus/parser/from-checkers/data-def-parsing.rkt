#lang racket
(provide add-all-dd)

; A DataDefinition is (cons Symbol Symbol)

; ds->dd : String -> DataDefinition
(define (ds->dd ds)
  (let* ([data-def (regexp-match
                    #px"(?mi:^[\\s;]*an?\\s+(.*\\S)\\s+is\\s+(?:an?\\s+)?(.*\\S)\\s*$)"
                    ds)]
         [from-type (second data-def)]
         [to-type (third data-def)])
    (cons (string->symbol (string-downcase from-type))
          (string->symbol (string-downcase to-type)))))

(define (add-all-dd dds env)
  (append (map ds->dd dds) env))