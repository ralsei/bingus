#lang racket
(require "from-checkers/datadef.rkt"
         "../ast.rkt"
         "../util.rkt")

(provide read-file-with-lang
         parse-checks
         checkers-dds->bingus-system
         checkers-polysigs->bingus-signature)

(define (read-until-eof prt)
  (let loop ([r (read prt)])
    (cond [(eof-object? r) '()]
          [else (cons r (loop (read prt)))])))

(define (read-file-with-lang prt)
  (define prog
    (parameterize ([read-accept-reader #t])
      (read-until-eof prt)))
  (match prog
    ;; sigh
    [`(module ,_ ,_ (#%module-begin ,xs ...)) xs]
    [`((module ,_ ,_ (#%module-begin ,xs ...))) xs]
    ;; not a module (read from defn window or sth)
    [x x]))

;; assuming all checks take the form (check-expect (NAME INPUTS ...) OUTPUT)
(define (parse-checks prog fn-name)
  (for/list ([l (in-list prog)]
             #:when (match l
                      [`(check-expect (,(== fn-name) ,_ ...) ,_) #t]
                      [_ #f]))
    (match l
      [`(check-expect ,actual ,expected) (check^ actual expected)])))

(define (parse-struct-fields prog product-name product-field-sigs)
  (for/list ([l (in-list prog)]
             #:when (begin
                      (match l
                        [`(define-struct ,(== (string->symbol product-name) ) ,_ ...) #t]
                        [_ #f])))
    (match l
      [`(define-struct ,_ ,flds)
       (map product-field$
            (map ~a flds)
            (map (λ (x) (checkers-pattern->bingus-pattern x prog)) product-field-sigs))])))

(define (checkers-pattern->bingus-pattern dp [prog #f])
  (match dp
    [(data-id 'number) (number-atom$)]
    [(data-id 'string) (string-atom$)]
    [(data-id 'boolean) (boolean-atom$)]
    [(data-id 'empty) (empty$)]
    [(data-id 'true) (singleton-atom$ #t)]
    [(data-id 'false) (singleton-atom$ #f)]
    [(data-id name) (~a name)]
    [(data-literal val) (singleton-atom$ val)]
    [(data-make 'cons `(,a ,d))
     (cons$ (checkers-pattern->bingus-pattern a prog)
            (checkers-pattern->bingus-pattern d prog))]
    [(data-make proc args)
     (cond [(not prog) (error 'checkers-pattern->bingus-pattern
                              "attempted to parse data-make without a read program")]
           [else
            (define prod-name (string-trim (~a proc) "make-"))
            (define flds (parse-struct-fields prog prod-name args))
            ;; HACK: what
            (product$ prod-name (car flds))])]
    [(data-app _ _)
     (error 'checkers-datadef->bingus-datadef "type applications unsupported in BSL")]))

(define (checkers-datadef->bingus-datadef name dd prog)
  (match-define (datadef '() '() data-patterns) dd)

  (defn$ (~a name)
    (cond [(empty? (rest data-patterns))
           (checkers-pattern->bingus-pattern (first data-patterns) prog)]
          [else (sum$ (map (λ (x)
                             (sum-case$ (checkers-pattern->bingus-pattern x prog)))
                           data-patterns))])))

(define (checkers-dds->bingus-system dds prog)
  (for/list ([(k v) (in-hash dds)])
    (checkers-datadef->bingus-datadef k v prog)))

(define (checkers-polysigs->bingus-signature polysigs fn-name)
  ; don't need type applications for BSL.
  ; also there should only be one thing in the output list
  (match-define (polysig _ _ ins (list out))
    (for/first ([s (in-list polysigs)]
                #:when (equal? (polysig-name s) fn-name))
      s))
  ;; XXX: ...how does this process sum types?
  (function$ (map (compose checkers-pattern->bingus-pattern car) ins)
             (checkers-pattern->bingus-pattern out)
             #f))
