#lang racket
(require rackunit)
(require (only-in racket/list filter-map))
(require (only-in racket/sandbox sandbox-memory-limit make-evaluator))
(require (only-in lang/htdp-beginner struct?))
(require "grouped.rkt"
         "err-msg.rkt"
         "permute.rkt")
(provide list=-no-order?
         (struct-out data-id)
         (struct-out data-app)
         (struct-out data-literal)
         (struct-out data-make)
         pattern-subst pattern-has? pattern-alpha=? pattern-rename infer-univs
         parse-data-pattern
         (struct-out datadef)
         datadef=?
         parse-datadefs reduce-patterns reduce-to-pattern reduce-to-patterns
         (struct-out polysig)
         parse-polysig parse-polysigs
         reduce-polysig polysig=?
         absent-polysig? lookup-polysig)

; list=-no-order? : [ListOf X] [ListOf X] -> Boolean
; Compare two lists for element equal?-ity without regard for order
(define (list=-no-order? lst1 lst2)
  (define (histogram seq)
    (define h (make-hash))
    (for ([k seq]) (hash-update! h k add1 0))
    h)
  (equal? (histogram (in-list lst1)) (histogram (in-list lst2))))

; A DataPattern is one of:                   ; *Interpretation*:
; - (data-id Symbol)                         ; type name (such as image or temperature) or type variable (such as x or y)
; - (data-app Symbol [NEListOf DataPattern]) ; type application (such as [listof number])
; - (data-literal Anything)                  ; a fixed constant (such as in an enumeration)
; - (data-make Symbol [ListOf DataPattern])  ; a named function (such as make-posn or +) applied
; - String                                   ; uninterpreted/uninterpretable
(struct data-id      [name]        #:transparent)
(struct data-app     [rator rands] #:transparent)
(struct data-literal [value]       #:transparent)
(struct data-make    [rator rands] #:transparent)

; pattern-subst : [Symbol -> DataPattern] -> [DataPattern -> DataPattern]
(define (pattern-subst env)
  (define f (match-lambda
    [(data-id s)             (env s)]
    [(data-app rator rands)  (data-app rator (map f rands))]
    [(data-make rator rands) (data-make rator (map f rands))]
    [p                       p]))
  f)

(module+ test
  (check-equal? ((pattern-subst data-literal)
                 (data-make 'bar (list (data-app 'foo (list (data-id 'x)
                                                            (data-id 'y)))
                                       (data-literal 3))))
                (data-make 'bar (list (data-app 'foo (list (data-literal 'x)
                                                           (data-literal 'y)))
                                      (data-literal 3)))))

; pattern-has? : [Symbol -> Boolean] -> [DataPattern -> Boolean]
(define (pattern-has? pred)
  (define f (match-lambda
    [(data-id s)             (pred s)]
    [(data-app rator rands)  (ormap f rands)]
    [(data-make rator rands) (ormap f rands)]
    [_                       #f]))
  f)

(module+ test
  (check-true ((pattern-has? (lambda (s) (symbol=? s 'x)))
               (data-app 'foo (list (data-id 'x) (data-id 'y)))))
  (check-false ((pattern-has? (lambda (s) (symbol=? s 'z)))
                (data-app 'foo (list (data-id 'x) (data-id 'y))))))

; pattern-alpha=? : [ListOf [Pair Symbol Symbol]] DataPattern DataPattern -> Boolean
(define (pattern-alpha=? bindings p1 p2)
  (match* (p1 p2)
    [((data-id s1) (data-id s2))
     (and (member (cons s1 s2) bindings) ; `bindings` relates bound type variables
          #t)]
    [((data-app rator1 rands1) (data-app rator2 rands2))
     (and (symbol=? rator1 rator2) ; `bindings` does not relate type constructors
          (= (length rands1) (length rands2))
          (for/and ([rand1 rands1] [rand2 rands2])
            (pattern-alpha=? bindings rand1 rand2)))]
    [((data-literal v1) (data-literal v2))
     (equal? v1 v2)]
    [((data-make rator1 rands1) (data-make rator2 rands2))
     (and (symbol=? rator1 rator2) ; `bindings` does not relate term variables
          (= (length rands1) (length rands2))
          (for/and ([rand1 rands1] [rand2 rands2])
            (pattern-alpha=? bindings rand1 rand2)))]
    [((? string?) (? string?))
     (string=? p1 p2)]
    [(_ _) #f]))

(module+ test
  (check-true (pattern-alpha=? '((x . y) (y . z))
                               (data-id 'x)
                               (data-id 'y)))
  (check-true (pattern-alpha=?
               '((x . y) (y . z))
               (data-app 'foo (list (data-id 'x) (data-id 'y)))
               (data-app 'foo (list (data-id 'y) (data-id 'z)))))
  (check-true (pattern-alpha=?
               '((x . y) (y . z))
               (data-app 'foo (list (data-id 'y) (data-id 'y)))
               (data-app 'foo (list (data-id 'z) (data-id 'z)))))
  (check-false (pattern-alpha=? '((x . y) (y . z))
                                (data-literal 'x)
                                (data-literal 'y)))
  (check-false (pattern-alpha=?
                '((x . y) (y . z))
                (data-app 'foo (list (data-id 'x) (data-id 'y)))
                (data-app 'foo (list (data-id 'y) (data-id 'x)))))
  (check-false (pattern-alpha=?
                '((x . y) (y . z))
                (data-app 'foo (list (data-id 'x) (data-id 'x)))
                (data-app 'foo (list (data-id 'z) (data-id 'z))))))

; pattern-rename : [ListOf [Pair Symbol Symbol]] DataPattern -> [Maybe DataPattern]
(define (pattern-rename src-dst p)
  (define to-be-bound
    (set-subtract (for/set ([s-d src-dst]) (cdr s-d))
                  (for/set ([s-d src-dst]) (car s-d))))
  (and (not ((pattern-has? (lambda (s) (set-member? to-be-bound s))) p))
       ((pattern-subst (lambda (s)
                         (data-id (cond [(assoc s src-dst) => cdr]
                                        [else s]))))
        p)))

(module+ test
  (check-equal? (pattern-rename '((x . y) (y . z))
                                (data-app 'foo (list (data-id 'x) (data-id 'y))))
                (data-app 'foo (list (data-id 'y) (data-id 'z))))
  (check-equal? (pattern-rename '((x . y) (y . z))
                                (data-app 'foo (list (data-id 'x) (data-id 'z))))
                #f)
  (check-equal? (pattern-rename '((x . y) (y . x))
                                (data-app 'foo (list (data-id 'x) (data-id 'y))))
                (data-app 'foo (list (data-id 'y) (data-id 'x)))))

(define well-known-atomic-data-ids
  (set 'number 'string 'image 'color 'mode))
(define probable-data-ids
  (set-union well-known-atomic-data-ids
             (set 'boolean 'naturalnumber 'listofnumbers
                  'temperature
                  'year 'month 'day 'dateorder 'monthformat
                  'x 'y 'z)))

; read-single : String -> Datum
; Read a single datum from the given string. If the given string does not
; contain exactly one datum, throw an exception.
(define (read-single s)
  (define p (open-input-string s))
  (define d (read p))
  (cond [(eof-object? d) (error "No datum found in" s)]
        [(eof-object? (read p)) d]
        [else (error "Multiple data found in" s)]))

(module+ test
  (check-exn #px"^No datum found in \\\"\\\"$"
             (lambda () (read-single "")))
  (check-exn #px"^Multiple data found in \\\"\\(\\+ 2 3\\) 4\\\"$"
             (lambda () (read-single "(+ 2 3) 4")))
  (check-equal? (read-single "(+ 2 3)") '(+ 2 3)))

; literal-ok? : Anything -> Boolean
; Check if the given value is free of procedures and BSL structures.  This
; disallows signature-name (like Person) and parametric-signature-name (like
; PersonOf) created by do-define-struct in htdp-lib/lang/private/teach.rkt,
; because both Person and PersonOf satisfy procedure?
(define (literal-ok? v)
  (cond [(procedure? v) #f]
        [(struct? v) #f]
        [(void? v) #f]
        [(pair? v) (and (literal-ok? (car v)) (literal-ok? (cdr v)))]
        [else #t]))

(module+ test
  (check-true (literal-ok? 0))
  (check-true (literal-ok? 1))
  (check-true (literal-ok? "hi"))
  (check-true (literal-ok? (list 0 1 "hi")))
  (check-false (literal-ok? (void)))
  (check-false
   (literal-ok?
    ((make-evaluator 'lang/htdp-beginner
                     "(define-struct person [name age])")
     'Person)))
  (check-false
   (literal-ok?
    ((make-evaluator 'lang/htdp-beginner
                     "(define-struct person [name age])")
     'PersonOf)))
  (check-false
   (literal-ok?
    ((make-evaluator 'lang/htdp-beginner
                     "(define-struct person [name age])")
     '(list Person Person))))
  (check-false
   (literal-ok?
    ((make-evaluator 'lang/htdp-beginner
                     "(define-struct person [name age])")
     '(list 0 (make-posn Person Person)))))
  (check-false
   (literal-ok?
    ((make-evaluator 'lang/htdp-beginner
                     "(define-struct person [name age])")
     '(make-posn "" 0)))))

; infer-univs : DataPattern -> [Set Symbol]
; Look for stuff like (data-id 'x)
(define infer-univs
  (match-lambda
    [(data-id (and (or 'x 'y 'z) s)) (set s)]
    [(data-app _ ps) (apply set-union (map infer-univs ps))]
    [(data-make _ ps) (apply set-union (set) (map infer-univs ps))]
    [_ (set)]))

; arrow? : String -> Boolean
(define (arrow? str) (regexp-match? #px"^-+>$" str))

; not-arrow? : String -> Boolean
(define (not-arrow? str) (not (arrow? str)))

; normalize-data-name : String -> Symbol
; Used for the Symbol in data-id (such as 'number) and data-app (such as 'listof)
; but not data-make (such as 'make-posn, because that's case-sensitive)
(define (normalize-data-name s)
  (string->symbol (regexp-replace* #rx"(?:^|-)(?:(?:ne)?lists?-?of(?:-|$))+"
                                   (string-downcase s)
                                   (lambda (m) (string-replace m "-" "")))))

(module+ test
  (check-equal? (normalize-data-name "List-of-lists-Of-Numbers") 'listoflistsofnumbers)
  (check-equal? (normalize-data-name "list-of-ne-lists-of-numbers") 'listofnelistsofnumbers)
  (check-equal? (normalize-data-name "nelist-of-lists-ofnumbers") 'nelistoflists-ofnumbers)
  (check-equal? (normalize-data-name "nelist-of-listof-numbers") 'nelistoflistofnumbers)
  (check-equal? (normalize-data-name "listof-string") 'listofstring)
  (check-equal? (normalize-data-name "iMaGe") 'image))

; parse-data-pattern : String [Datum -> Anything] -> DataPattern
; Try to interpret the given string as a data pattern.
; If the given evaluator throws an exception, we don't make a data-literal
(define (parse-data-pattern str eval)
  (define (loop s) (parse-data-pattern s eval))
  (define trimmed-str (string-trim str))
  (define normalized-name (normalize-data-name trimmed-str))
  (define (handler . _)
    (cond
      [(regexp-match? #px"^[^][(){}\",.'`|;:#\\s]+$" trimmed-str)
       (data-id normalized-name)]
      [(regexp-match #px"^\\[\\s*(\\S.*)\\]$" trimmed-str)
       => (lambda (m)
            (match (grouped-string-split (cadr m))
              [(list (? not-arrow? args) ..1
                     (? arrow?)
                     (? not-arrow? return-type) ..1)
               (data-app '-> (append (map loop args)
                                     (list (loop (string-join return-type)))))]
              [(list (? not-arrow? rator)
                     (? not-arrow? rands) ..1)
               #:when (regexp-match? #px"^[^][(){}\",.'`|;:#\\s]+$" rator)
               (data-app (normalize-data-name rator) (map loop rands))]
              [_ trimmed-str]))]
      [(regexp-match #px"^\\(\\s*([^][(){}\",'`|;#\\s]+)(.*)\\)$" trimmed-str)
       => (lambda (m)
            (define rands (map loop (grouped-string-split (caddr m))))
            (data-make (string->symbol (cadr m)) rands))]
      [else trimmed-str]))
  (cond
    [(set-member? probable-data-ids normalized-name)
     (data-id normalized-name)]
    [else (with-handlers ([exn:fail? handler])
            (let ([val (eval (datum->syntax #f (read-single str)))])
              (if (literal-ok? val) (data-literal val) (handler))))]))
(define (base-eval x) (eval x (make-base-namespace)))

(module+ test
  (check-equal? (parse-data-pattern "  (+ 1 Nat)  " base-eval)
                (data-make '+ (list (data-literal 1)
                                    (data-id 'nat))))
  (check-equal? (parse-data-pattern "(make-point Number Number)" base-eval)
                (data-make 'make-point (list (data-id 'number)
                                             (data-id 'number))))
  (check-equal? (parse-data-pattern "  #true  " base-eval)
                (data-literal true))
  (check-equal? (parse-data-pattern "  \"red\"  " base-eval)
                (data-literal "red"))
  (check-equal? (parse-data-pattern "number less than 100  " base-eval)
                "number less than 100")
  (check-equal? (parse-data-pattern "  [List-of Number]  " base-eval)
                (data-app 'listof (list (data-id 'number))))
  (check-equal? (parse-data-pattern "  [List]  " base-eval)
                "[List]")
  (check-equal?
   (parameterize ([sandbox-memory-limit 100])
     (parse-data-pattern "[Pair Person PersonOf]"
                         (make-evaluator 'lang/htdp-beginner
                                         "(define-struct person [name age])")))
   (data-app 'pair (list (data-id 'person) (data-id 'personof)))))

; parse-data-cases : [ListOf String] -> [Pair [ListOf String] [ListOf String]]
; Split the given list of lines into a list of commented indented bullet items
; at the beginning and the list of remaining lines
(define (parse-data-cases lines)
  (cond
    [(and (pair? lines)
          (regexp-match? #px"^[\\s;]*$" (car lines)))
     (parse-data-cases (cdr lines))]
    [(and (pair? lines)
          (regexp-match #px"^(\\s*;[\\s;]*-\\s*)(\\S.*?)\\s*$" (car lines)))
     => (lambda (m)
          (define indent (pregexp (format "^[\\s;]{~a,}(\\S.*?)\\s*$"
                                          (string-length (cadr m)))))
          (let accum ([rest (cdr lines)] [seen (caddr m)])
            (cond
              [(null? rest)
               (cons (list seen) '())]
              [(regexp-match? #px"^[\\s;]*$" (car rest))
               (accum (cdr rest) (string-append seen "\n"))]
              [(regexp-match indent (car rest))
               => (lambda (m) (accum (cdr rest)
                                     (string-append seen "\n" (cadr m))))]
              [else
               (define res (parse-data-cases rest))
               (cons (cons seen (car res)) (cdr res))])))]
    [else (cons '() lines)]))

(module+ test
  (check-equal? (parse-data-cases (list "; - foo"
                                        ";   bar"
                                        "; - baz"
                                        "blah"))
                (cons (list "foo\nbar" "baz") (list "blah")))
  (check-equal? (parse-data-cases (list "; - foo"
                                        ";"
                                        ";    bar"
                                        ""
                                        "; - baz"
                                        "blah"))
                (cons (list "foo\n\nbar\n" "baz") (list "blah")))
  (check-equal? (parse-data-cases (list "; - foo "
                                        " ;   bar"
                                        ""
                                        ";;;;BAR "
                                        ";"
                                        "; - baz"
                                        ";  blah"))
                (cons (list "foo\nbar\n\nBAR\n" "baz") (list ";  blah")))
  (check-equal? (parse-data-cases (list "; - foo "
                                        " ;   bar"
                                        ";;;;BAR "
                                        "; - baz"))
                (cons (list "foo\nbar\nBAR" "baz") (list)))
  (check-equal? (parse-data-cases (list "; - foo "
                                        " ;   bar"
                                        ";;;;BAR "))
                (cons (list "foo\nbar\nBAR") (list)))
  (check-equal? (parse-data-cases (list "; - foo "))
                (cons (list "foo") (list))))

; A DataDefinition is (datadef [ListOf String] [ListOf Symbol] [NEListOf DataPattern])
(struct datadef [error arglist body] #:transparent)

; datadef=? : DataDefinition DataDefinition -> Boolean
(define (datadef=? datadef1 datadef2)
  (and (equal? (datadef-error datadef1)
               (datadef-error datadef2))
       (= (length (datadef-arglist datadef1))
          (length (datadef-arglist datadef2)))
       (list=-no-order?
        (datadef-body datadef1)
        (let ([src-dst (map cons (datadef-arglist datadef2)
                                 (datadef-arglist datadef1))])
          (for/list ([p (datadef-body datadef2)])
            (pattern-rename src-dst p))))))

(module+ test
  (check-true (datadef=?
               (datadef '() '(x)
                        (list (data-literal '())
                              (data-make 'cons (list (data-id 'x)
                                                     (data-app 'listof (list (data-id 'x)))))))
               (datadef '() '(number)
                        (list (data-literal '())
                              (data-make 'cons (list (data-id 'number)
                                                     (data-app 'listof (list (data-id 'number))))))))))

; parse-dds! : [MutableHash Symbol DataDefinition] [ListOf String] [Datum -> Anything] -> Void
; Extract data definitions from comments in the given lines.
; Use the given evaluator to interpret data literals.
(define (hash-add! h key new)
  (hash-update! h key
    (lambda (old)
      (cond
        [(not old) new]
        [(datadef=? old new) old]
        [else
         (datadef (cons "Multiple inconsistent definitions" (datadef-error old))
                  (datadef-arglist old)
                  (datadef-body old))]))
    #f))
(define (parse-dds! h lines eval)
  (cond
    [(null? lines) (void)]
    [(regexp-match #px"(?i:^\\s*;[\\s;]*an?\\s+(?:([^][(){}\",'`|;#\\s]+)|\\[\\s*([^][(){}\",'`|;#\\s]+(?:\\s+[^][(){}\",'`|;#\\s]+)+)\\s*\\])\\s+is\\s+(?:an?\\s+)?(.*)$)"
                   (car lines))
     => (lambda (m)
          (define head-strings (string-split (or (cadr m) (caddr m))))
          (match-define (cons head arglist) (map normalize-data-name head-strings))
          (when (member head '(x y z))
            (error (format "You made a data definition named \"~a\". Usually \"~a\" only names a data definition that is an input to another data definition or to a signature. Your data definition is not an input, but rather takes effect throughout the entire program, so it is likely to be confusing, and you should rename it." (car head-strings) (car head-strings))))
          (define err
            (cond [(check-duplicates arglist)
                   => (lambda (dup)
                        (list (format "Found a data definition variable that is used more than once: ~a" dup)))]
                  [else '()]))
          (define rhs (cadddr m))
          (define (non-itemization)
            (hash-add! h head (datadef err arglist (list (parse-data-pattern rhs eval))))
            (parse-dds! h (cdr lines) eval))
          (cond
            [(regexp-match? #px"(?i:^\\W*one\\s+of\\W*$)" rhs)
             (match-let ([(cons cases rest) (parse-data-cases (cdr lines))])
               (if (pair? cases)
                 (begin (hash-add! h head
                          (datadef err arglist
                            (for/list ([c (in-list cases)])
                              (parse-data-pattern
                               (regexp-replace #px"(?i:^\\s*an?\\s)" c "")
                               eval))))
                        (parse-dds! h rest eval))
                 (non-itemization)))]
            [(and (not (null? (cdr lines)))
                  (regexp-match? #px"(?i:^\\W*structure\\W*$)" rhs)
                  (regexp-match #px"(?i:^\\s*;[\\s;]*(\\(.*\\))\\s*$)" (cadr lines)))
             ; An Editor is a structure:
             ;   (make-editor String String)
             => (lambda (ms)
                  (hash-add! h head
                    (datadef err arglist (list (parse-data-pattern (cadr ms) eval))))
                  (parse-dds! h (cddr lines) eval))]
            [(regexp-match #px"(?i:^\\W*structure\\b[\\s:;.,]*(.*\\w.*)$)" rhs)
             ; An Editor is a structure: (make-editor String String)
             => (lambda (ms)
                  (hash-add! h head
                    (datadef err arglist (list (parse-data-pattern (cadr ms) eval))))
                  (parse-dds! h (cdr lines) eval))]
            [else (non-itemization)]))]
    [else (parse-dds! h (cdr lines) eval)]))

; make-datadef-hash : [ListOf Symbol] -> [MutableHash Symbol DataDefinition]
; Initialize the mutable hash table of parsed data definitions using the given
; list of built-in data definitions ('(boolean) at the start of the semester,
; '(boolean listofnumbers) after lecture18)
(define built-in-datadefs
  (hash 'boolean (datadef '() '() (list (data-literal #t)
                                        (data-literal #f)))
        'listofnumbers (datadef '() '() (list (data-app 'listof (list (data-id 'number)))))))
(define (make-datadef-hash built-in)
  (make-hash (for/list ([k (in-list built-in)])
               (cons k (hash-ref built-in-datadefs k)))))

; parse-dds : [ListOf String] [Datum -> Anything] -> [Hash Symbol [NEListOf DataPattern]]
; Extract data definitions from comments in the given lines.
; Use the given evaluator to interpret data literals.
; If a name is redefined differently, omit it from the hash.
(define (parse-dds lines eval)
  (define h (make-datadef-hash '(boolean)))
  (parse-dds! h lines eval)
  (for/hash ([(k datadef) (in-hash h)]
             #:when (and (null? (datadef-error datadef))
                         ; Throw away parametric data definitions
                         (null? (datadef-arglist datadef))))
    (values k (datadef-body datadef))))

(module+ test
  (check-equal? (parse-dds (list ""
                                 "; A Time is one of:"
                                 "; - a number less than 100"
                                 "; - a number at least 100"
                                 ""
                                 "; A Boolean is one of:"
                                 "; - #false"
                                 "; - #true"
                                 ""
                                 "; launch-rocket : Time -> Image")
                           base-eval)
                (hash 'time (list "number less than 100"
                                  "number at least 100")
                      'boolean (list (data-literal true)
                                     (data-literal false))))
  (check-equal? (parse-dds (list "; A Time is one of:"
                                 "; - a number at least 100"
                                 "; - a number less than 100"
                                 "; A Time is one of:"
                                 ";    - a number less than 100"
                                 ";    - a number at least 100")
                           base-eval)
                (hash 'boolean (list (data-literal true)
                                     (data-literal false))
                      'time (list "number at least 100"
                                  "number less than 100")))
  (check-equal? (parse-dds (list "; A Time is one of:"
                                 "; - a number less than 100"
                                 "; - a number at least 100"
                                 "; A Time is one of:"
                                 ";    - a number at least 200"
                                 ";    - a number less than 200")
                           base-eval)
                (hash 'boolean (list (data-literal true)
                                     (data-literal false))))
  (check-equal? (parse-dds (list "; An Editor is a structure:"
                                 ";   (make-editor String String)"
                                 "; interpretation (make-editor s t) describes an editor"
                                 "; whose visible text is (string-append s t) with"
                                 "; the cursor displayed between s and t")
                           base-eval)
                (hash 'boolean (list (data-literal true)
                                     (data-literal false))
                      'editor (list (data-make 'make-editor
                                               (list (data-id 'string)
                                                     (data-id 'string))))))
  (check-equal? (parse-dds (list "; An Editor is a structure: (make-editor String String)"
                                 "; interpretation (make-editor s t) describes an editor"
                                 "; whose visible text is (string-append s t) with"
                                 "; the cursor displayed between s and t")
                           base-eval)
                (hash 'boolean (list (data-literal true)
                                     (data-literal false))
                      'editor (list (data-make 'make-editor
                                               (list (data-id 'string)
                                                     (data-id 'string))))))
  (check-equal? (parse-dds (list "; An Editor is a structure:")
                           base-eval)
                (hash 'boolean (list (data-literal true)
                                     (data-literal false))
                      'editor (list "structure:")))
  (check-equal? (parse-dds (list "; A CoupleOfPoints is one of:"
                                 "; - (make-none)"
                                 "; - (make-one Point)"
                                 "; - (make-two Point Point)")
                           base-eval)
                (hash 'boolean (list (data-literal true)
                                     (data-literal false))
                      'coupleofpoints (list (data-make 'make-none empty)
                                            (data-make 'make-one (list (data-id 'point)))
                                            (data-make 'make-two (list (data-id 'point)
                                                                       (data-id 'point)))))))

; parse-datadefs : [ListOf String] [Datum -> Anything] -> [Hash Symbol DataDefinition]
; Extract data definitions (possibly polymorphic) from comments in the given lines.
; Use the given evaluator to interpret data literals.
(define (parse-datadefs lines eval #:built-in [built-in '(boolean)])
  (define h (make-datadef-hash built-in))
  (parse-dds! h lines eval)
  (for/hash
    ([(k datadef) (in-hash h)]
     #:when (or (null? (datadef-error datadef))
                (begin
                 (error
                   (string-join (cons (format "For the data definition \"~a\":" k)
                                      (datadef-error datadef))
                                "\n"))
                 #f)))
    (values k datadef)))

(module+ test
  (define sample-datadefs
    (hash 'boolean (datadef '() '()
                            (list (data-literal true)
                                  (data-literal false)))
          'listofnumbers (datadef '() '() (list (data-app 'listof (list (data-id 'number)))))
          'listof (datadef '() '(x)
                           (list (data-literal '())
                                 (data-make 'cons (list (data-id 'x)
                                                        (data-app 'listof (list (data-id 'x)))))))
          'maybe (datadef '() '(x)
                          (list (data-make 'make-none '())
                                (data-id 'x)))
          'nelistof (datadef '() '(x)
                             (list (data-make 'cons (list (data-id 'x)
                                                          (data-literal '())))
                                   (data-make 'cons (list (data-id 'x)
                                                          (data-app 'nelistof (list (data-id 'x)))))))
          'frame (datadef '() '() (list (data-id 'time)))
          'green (datadef '() '() (list (data-literal "green")))
          'trafficlight (datadef '() '()
                                 (list (data-literal "red")
                                       (data-id 'green)
                                       (data-literal "yellow")))))
  (check-equal? (parse-datadefs #:built-in '(boolean listofnumbers)
                                (list "; A [ListOf X] is one of:"
                                      "; - empty"
                                      "; - (cons X [ListOf X])"
                                      "; A [ListOf Number] is one of:"
                                      "; - empty"
                                      "; - (cons Number [ListOf Number])"
                                      "; A [Maybe X] is one of:"
                                      "; - (make-none)"
                                      "; - X"
                                      ";; A [NEListOf X] is one of:"
                                      ";; - (cons X empty)"
                                      ";; - (cons X [NEListOf X])"
                                      "; A [Foo X X] is one of:"
                                      "; - \"pink\""
                                      "; - \"purple\""
                                      "; A Time is one of:"
                                      "; - a number less than 100"
                                      "; - a number at least 100"
                                      "; A Frame is Time"
                                      "; A Time is one of:"
                                      "; - a number at least 200"
                                      "; - a number less than 200"
                                      "; A Green is \"green\""
                                      "; A TrafficLight is one of:"
                                      "; - \"red\""
                                      "; - Green"
                                      "; - \"yellow\"")
                                (make-evaluator 'lang/htdp-beginner "(define-struct none [])"))
                sample-datadefs)
  (check-equal? (parse-datadefs #:built-in '(boolean listofnumbers)
                                (list "; A List-Of-Numbers is a [List-of Number]"
                                      "; A Listof-Lists-oF-Numbers is a [List-of Listofnumbers]")
                                base-eval)
                (hash 'boolean
                      (datadef '() '()
                               (list (data-literal true)
                                     (data-literal false)))
                      'listofnumbers
                      (datadef '() '()
                               (list (data-app 'listof (list (data-id 'number)))))
                      'listoflistsofnumbers
                      (datadef '() '()
                               (list (data-app 'listof (list (data-id 'listofnumbers))))))))

; reduce-type : [Hash Symbol [NEListOf DataPattern]] Symbol -> [NEListOf DataPattern]
; Loop up the meaning of a data definition name, following data-id chains
(define (reduce-type dds t)
  (let loop ([t t] [seen (set)])
    (cond [(or (set-member? seen t)
               (set-member? well-known-atomic-data-ids t))
           (list (data-id t))]
          [(hash-ref dds t #f)
           => (lambda (patterns)
                (for*/list ([pattern (in-list patterns)]
                            [pattern^ (match pattern
                                        [(data-id t^)
                                         (in-list (loop t^ (set-add seen t)))]
                                        [_ (in-value pattern)])])
                  pattern^))]
          [else (list (data-id t))])))

(module+ test
  (define sample-dds
    (parse-dds (list "; A TrafficLight is one of:"
                     "; - \"red\""
                     "; - Green"
                     "; - \"yellow\""
                     "; A Green is \"green\""
                     "; A Time is one of:"
                     "; - a number less than 100"
                     "; - a number at least 100"
                     "; A Frame is Time"
                     "; A Foo is Bar"
                     "; A Bar is a Foo"
                     "; A Time is one of:"
                     "; - a number at least 200"
                     "; - a number less than 200"
                     "; A TrafficLight is one of:"
                     "; - \"red\""
                     "; - Green"
                     "; - \"yellow\"")
               base-eval))
  (check-equal? (reduce-type sample-dds 'frame)
                (list (data-id 'time)))
  (check-equal? (reduce-type sample-dds 'trafficlight)
                (list (data-literal "red")
                      (data-literal "green")
                      (data-literal "yellow")))
  (check-equal? (reduce-type sample-dds 'foo)
                (list (data-id 'foo))))

; extend-env : [ListOf X] [ListOf Y] [X -> Y] -> [X -> Y]
(define (extend-env arglist rands default)
  (define h (for/hash ([arg arglist] [rand rands]) (values arg rand)))
  (lambda (x) (hash-ref h x (lambda () (default x)))))

; reduce-patterns : [Hash Symbol DataDefinition] [NEListOf DataPattern] -> [NEListOf DataPattern]
(define (reduce-patterns datadefs ps [seen (set)])
  (for*/list ([p ps]
              [p^ (reduce-to-patterns datadefs p seen)])
    p^))

; reduce-to-pattern : [Hash Symbol DataDefinition] DataPattern -> DataPattern
(define (reduce-to-pattern datadefs p [seen (set)])
  (match p
    [(data-id s)
     (match (or (set-member? seen s)
                (set-member? well-known-atomic-data-ids s)
                (hash-ref datadefs s #f))
       [(datadef _ '() (list p))
        (reduce-to-pattern datadefs p (set-add seen s)) ]
       [_ p])]
    [(data-app s rands)
     (match (or (set-member? seen s)
                (set-member? well-known-atomic-data-ids s)
                (hash-ref datadefs s #f))
       [(datadef _ arglist (list p))
        #:when (= (length arglist) (length rands))
        (define subst (pattern-subst (extend-env arglist rands data-id)))
        (reduce-to-pattern datadefs (subst p) (set-add seen s))]
       [_ (data-app s (for/list ([rand rands])
                        (reduce-to-pattern datadefs rand seen)))])]
    [(data-make s rands)
     (data-make s (for/list ([rand rands])
                    (reduce-to-pattern datadefs rand seen)))]
    [_ p]))

; reduce-to-patterns : [Hash Symbol DataDefinition] DataPattern -> [NEListOf DataPattern]
(define (reduce-to-patterns datadefs p [seen (set)])
  (match p
    [(data-id s)
     (match (or (set-member? seen s)
                (set-member? well-known-atomic-data-ids s)
                (hash-ref datadefs s #f))
       [(datadef _ '() body)
        (reduce-patterns datadefs body (set-add seen s))]
       [_ (list p)])]
    [(data-app s rands)
     (match (or (set-member? seen s)
                (set-member? well-known-atomic-data-ids s)
                (hash-ref datadefs s #f))
       [(datadef _ arglist body)
        #:when (= (length arglist) (length rands))
        (define subst (pattern-subst (extend-env arglist rands data-id)))
        (reduce-patterns datadefs (map subst body) (set-add seen s))]
       [_ (list (data-app s (for/list ([rand rands])
                              (reduce-to-pattern datadefs rand seen))))])]
    [(data-make s rands)
     (list (data-make s (for/list ([rand rands])
                          (reduce-to-pattern datadefs rand seen))))]
    [_ (list p)]))

(module+ test
  (check-equal? (reduce-to-patterns sample-datadefs
                                    (data-app 'listof (list (data-id 'string))))
                (list
                 (data-literal '())
                 (data-make 'cons (list (data-id 'string)
                                        (data-app 'listof (list (data-id 'string))))))))

; A Signature is (signature Symbol [ListOf [NEListOf DataPattern]] [NEListOf DataPattern])
(struct signature [name args return-type] #:transparent)

; A PolySig is (polysig Symbol [Set Symbol] [ListOf [NEListOf DataPattern]] [NEListOf DataPattern])
(struct polysig [name univs args return-type] #:transparent)

; parse-signature : String [Datum -> Anything] -> [Maybe Signature]
(define (parse-signature line eval)
  (define m (regexp-match #px"^[\\s;]*([^][(){}\",'`|;#\\s]+)\\s*:\\s*(\\S.*?)\\s*--*>\\s*(\\S.*?)\\s*$" line))
  (and m
       (signature (string->symbol (cadr m))
                  (map (lambda (s) (list (parse-data-pattern s eval)))
                       (grouped-string-split (caddr m)))
                  (list (parse-data-pattern (cadddr m) eval)))))

; parse-polysig : String [Datum -> Anything] -> [Maybe PolySig]
(define (parse-polysig line eval)
  ; Square-bracketed univ names can only have uppercase letters
  (match (regexp-match #px"^[\\s;]*([^][(){}\",'`|;#\\s]+)\\s*:\\s*((?:\\[[[:upper:]\\s]*\\]\\s*|\\{[^][(){}\",'`|;#]*\\}\\s*)*)(\\S.*->.*\\S)\\s*$" line)
    [(list _ name univs (app grouped-string-split
                          (list (? not-arrow? args) ..1
                                (? arrow?)
                                (? not-arrow? return-type) ..1)))
     (define parsed-args (for/list ([arg args]) (parse-data-pattern arg eval)))
     (define parsed-return-type (parse-data-pattern (string-join return-type) eval))
     (define parsed-univs
       (if (string=? "" univs)
         (apply set-union
                (infer-univs parsed-return-type)
                (map infer-univs parsed-args))
         (for/set ([univ (string-split (string-downcase univs) #px"[][{}\\s]+")])
           (string->symbol univ))))
     (polysig (string->symbol name)
              parsed-univs
              (map list parsed-args)
              (list parsed-return-type))]
    [#f #f]))

; parse-signatures : [ListOf String] [Datum -> Anything] -> [ListOf Signature]
(define (parse-signatures lines eval)
  (for*/list ([line lines]
              #:when (regexp-match? #px"^\\s*;" line)
              [s (in-value (parse-signature line eval))]
              #:when s)
    s))
(define sample-signatures
  (parse-signatures (list "; launch-rocket : Time -> Image"
                          "; take : NaturalNumber [ListOf X] -> [ListOf X]"
                          "; draw-tl : TrafficLight -> Image"
                          "")
                    base-eval))

(module+ test
  (check-equal? sample-signatures
                (list (signature 'launch-rocket
                                 (list (list (data-id 'time)))
                                 (list (data-id 'image)))
                      (signature 'take
                                 (list (list (data-id 'naturalnumber))
                                       (list (data-app 'listof (list (data-id 'x)))))
                                 (list (data-app 'listof (list (data-id 'x)))))
                      (signature 'draw-tl
                                 (list (list (data-id 'trafficlight)))
                                 (list (data-id 'image))))))

; parse-polysigs : [ListOf String] [Datum -> Anything] -> [ListOf PolySig]
(define (parse-polysigs lines eval)
  (for*/list ([line lines]
              #:when (regexp-match? #px"^\\s*;" line)
              [s (in-value (parse-polysig line eval))]
              #:when s)
    s))
(define sample-polysigs
  (parse-polysigs (list "; launch-rocket : Time -> Image"
                        "; take : NaturalNumber [ListOf X] -> [ListOf X]"
                        "; draw-tl : TrafficLight -> Image"
                        "; filter : [X] [X -> Boolean] [ListOf X] -> [ListOf X]"
                        "; map : {X Y} [X -> Y] [ListOf X] -> [ListOf Y]"
                        "")
                  base-eval))

(module+ test
  (check-equal? sample-polysigs
                (list (polysig 'launch-rocket (set)
                               (list (list (data-id 'time)))
                               (list (data-id 'image)))
                      (polysig 'take (set 'x)
                               (list (list (data-id 'naturalnumber))
                                     (list (data-app 'listof (list (data-id 'x)))))
                               (list (data-app 'listof (list (data-id 'x)))))
                      (polysig 'draw-tl (set)
                               (list (list (data-id 'trafficlight)))
                               (list (data-id 'image)))
                      (polysig 'filter (set 'x)
                               (list (list (data-app '-> (list (data-id 'x) (data-id 'boolean))))
                                     (list (data-app 'listof (list (data-id 'x)))))
                               (list (data-app 'listof (list (data-id 'x)))))
                      (polysig 'map (set 'x 'y)
                               (list (list (data-app '-> (list (data-id 'x) (data-id 'y))))
                                     (list (data-app 'listof (list (data-id 'x)))))
                               (list (data-app 'listof (list (data-id 'y)))))))
  (check-equal?
   (parse-polysigs (list "; launch-rocket : Time -> Image"
                         "; take :   {   x   }   [   X   ]   NaturalNumber [  ListOf   X]->[ListOf X  ]"
                         "; draw-tl : TrafficLight->Image"
                         "; filter : [X][X->Boolean][ListOf X]---->[ListOf X]"
                         "; map : {X}{Y}[X->Y][ListOf X]->[ListOf Y]"
                         "")
                   base-eval)
   sample-polysigs))

; reduce-signature : [Hash Symbol [NEListOf DataPattern]] Signature -> Signature
(define (reduce-signature dds s)
  (define reduce (match-lambda [(list (data-id t))
                                (reduce-type dds t)]
                               [v v]))
  (signature (signature-name s)
             (map reduce (signature-args s))
             (reduce (signature-return-type s))))

; reduce-polysig : [Hash Symbol DataDefinition] PolySig -> PolySig
(define (reduce-polysig datadefs sig)
  (define univs (polysig-univs sig))
  (define src-dst ; rename all type variables, in case they are used free in datadefs
    (for/list ([src univs]) (cons src (gensym src)))) ; maybe use string->uninterned-symbol instead of gensym?
  (define univs^ (for/set ([s-d src-dst]) (cdr s-d)))
  (define (reduce ps)
    (define ps^ (for/list ([p ps]) (pattern-rename src-dst p)))
    (reduce-patterns datadefs ps^ univs^))
  (polysig (polysig-name sig)
           univs^
           (map reduce (polysig-args sig))
           (reduce (polysig-return-type sig))))

; signature=? : Signature Signature -> Boolean
(define (signature=? s t)
  (and (symbol=? (signature-name s) (signature-name t))
       (= (length (signature-args s)) (length (signature-args t)))
       (for/and ([arg1 (in-list (signature-args s))]
                 [arg2 (in-list (signature-args t))])
         (list=-no-order? arg1 arg2))
       (list=-no-order? (signature-return-type s) (signature-return-type t))))

; polysig=? : PolySig PolySig -> Boolean
(define (polysig=? sig1 sig2 #:permute [perm #f])
  (and (symbol=? (polysig-name sig1)
                 (polysig-name sig2))
       (= (set-count (polysig-univs sig1))
          (set-count (polysig-univs sig2)))
       (= (length (polysig-args sig1))
          (length (polysig-args sig2)))
       (let ([univs1 (set->list (polysig-univs sig1))]
             [args2 (permute-list perm (polysig-args sig2))])
         (for/or ([univs2 (in-permutations (set->list (polysig-univs sig2)))])
           (define src-dst (map cons univs2 univs1))
           (define (rename p) (pattern-rename src-dst p))
           (and (for/and ([arg1 (in-list (polysig-args sig1))]
                          [arg2 (in-list args2)])
                  (list=-no-order? arg1 (map rename arg2)))
                (list=-no-order? (polysig-return-type sig1)
                                 (map rename (polysig-return-type sig2))))))))

(module+ test
  (check-true
   (polysig=? (polysig 'map (set 'x 'y)
                       (list (list (data-app '-> (list (data-id 'x) (data-id 'y))))
                             (list (data-app 'listof (list (data-id 'x)))))
                       (list (data-app 'listof (list (data-id 'y)))))
              (polysig 'map (set 'y 'z)
                       (list (list (data-app '-> (list (data-id 'z) (data-id 'y))))
                             (list (data-app 'listof (list (data-id 'z)))))
                       (list (data-app 'listof (list (data-id 'y)))))))
  (check-true
   (polysig=? (polysig 'map (set 'x 'y)
                       (list (list (data-app '-> (list (data-id 'x) (data-id 'y))))
                             (list (data-app 'listof (list (data-id 'x)))))
                       (list (data-app 'listof (list (data-id 'y)))))
              (polysig 'map (set 'y 'z)
                       (list (list (data-app '-> (list (data-id 'y) (data-id 'z))))
                             (list (data-app 'listof (list (data-id 'y)))))
                       (list (data-app 'listof (list (data-id 'z)))))))
  (check-false
   (polysig=? (polysig 'map (set 'x 'y)
                       (list (list (data-app '-> (list (data-id 'x) (data-id 'y))))
                             (list (data-app 'listof (list (data-id 'x)))))
                       (list (data-app 'listof (list (data-id 'y)))))
              (polysig 'map (set 'y 'z)
                       (list (list (data-app '-> (list (data-id 'y) (data-id 'z))))
                             (list (data-app 'listof (list (data-id 'z)))))
                       (list (data-app 'listof (list (data-id 'y))))))))

; absent-signature? : [ListOf Signature] Symbol -> Boolean
(define (absent-signature? sigs unwanted)
  (not (for/or ([s (in-list sigs)])
         (symbol=? unwanted (signature-name s)))))

; absent-polysig? : [ListOf PolySig] Symbol -> Boolean
(define (absent-polysig? sigs unwanted)
  (not (for/or ([s (in-list sigs)])
         (symbol=? unwanted (polysig-name s)))))

; permute-polysig : Permutation PolySig -> PolySig
(define (permute-polysig perm sig)
  (if perm
    (polysig (polysig-name sig)
             (polysig-univs sig)
             (permute-list perm (polysig-args sig))
             (polysig-return-type sig))
    sig))

; lookup-signature : [Hash Symbol [NEListOf DataPattern]] [ListOf Signature]
;                    [Either Symbol Signature] -> Signature
(define (lookup-signature dds sigs wanted)
  (define name
    (cond [(signature? wanted) (signature-name wanted)]
          [(symbol? wanted) wanted]))
  (define results
    (for/list ([s (in-list sigs)]
               #:when (symbol=? name (signature-name s)))
      (reduce-signature dds s)))
  (cond
    [(null? results)
     (error (err-msg-signature-nonexistent name))]
    [(for/and ([result (in-list (cdr results))])
       (signature=? result (car results)))
     (when (and (signature? wanted)
                (not (signature=? (car results) (reduce-signature dds wanted))))
       (error (err-msg-signature-incorrect name)))
     (car results)]
    [else
     (error (err-msg-signature-multiple name))]))

(module+ test
  (check-equal? (lookup-signature sample-dds sample-signatures 'draw-tl)
                (signature 'draw-tl
                           (list (list (data-literal "red")
                                       (data-literal "green")
                                       (data-literal "yellow")))
                           (list (data-id 'image))))
  (check-exn #px"missing.*signature.*next-tl" (lambda ()
                                                (lookup-signature sample-dds sample-signatures 'next-tl)))
  (check-exn #px"signature.*draw-tl.*incorrect" (lambda ()
                                                  (lookup-signature sample-dds sample-signatures
                                                                    (signature 'draw-tl
                                                                               (list (list (data-literal "red")
                                                                                           (data-literal "green")
                                                                                           (data-literal "yellow")))
                                                                               (list (data-id 'string))))))
  (check-exn #px"missing.*signature.*next-tl" (lambda ()
                                                (lookup-signature sample-dds sample-signatures
                                                                  (signature 'next-tl
                                                                             (list (list (data-literal "red")
                                                                                         (data-literal "green")
                                                                                         (data-literal "yellow")))
                                                                             (list (data-id 'string))))))
  (check-not-exn (lambda ()
                   (lookup-signature sample-dds sample-signatures
                                     (signature 'launch-rocket
                                                (list (list (data-id 'time)))
                                                (list (data-id 'image))))))
  (check-not-exn (lambda ()
                   (lookup-signature sample-dds sample-signatures
                                     (signature 'launch-rocket
                                                (list (list (data-id 'frame)))
                                                (list (data-id 'image)))))))

; lookup-polysig : [Hash Symbol DataDefinition] [ListOf PolySig]
;                  [Either Symbol PolySig] -> PolySig
(define (lookup-polysig datadefs sigs wanted #:permute [perm #f])
  (define name
    (cond [(polysig? wanted) (polysig-name wanted)]
          [(symbol? wanted) wanted]))
  (define results
    (for/list ([s (in-list sigs)]
               #:when (symbol=? name (polysig-name s)))
      (reduce-polysig datadefs s)))
  (cond
    [(null? results)
     (error (err-msg-signature-nonexistent name))]
    [(for/and ([result (in-list (cdr results))])
       (polysig=? result (car results)))
     ; FIXME: What if people write a polymorphic signature and instances of it?
     (when (and (polysig? wanted)
                (not (polysig=? (car results)
                                (reduce-polysig datadefs wanted)
                                #:permute perm)))
       (error (err-msg-signature-incorrect name)))
     (permute-polysig (inverse-permutation perm) (car results))]
    [else
     (error (err-msg-signature-multiple name))]))

(module+ test
  (check-equal? (lookup-polysig sample-datadefs sample-polysigs 'draw-tl)
                (polysig 'draw-tl (set)
                         (list (list (data-literal "red")
                                     (data-literal "green")
                                     (data-literal "yellow")))
                         (list (data-id 'image))))
  (check-exn #px"missing.*signature.*next-tl" (lambda ()
                                                (lookup-polysig sample-datadefs sample-polysigs 'next-tl)))
  (check-exn #px"signature.*draw-tl.*incorrect" (lambda ()
                                                  (lookup-polysig sample-datadefs sample-polysigs
                                                                  (polysig 'draw-tl (set)
                                                                           (list (list (data-literal "red")
                                                                                       (data-literal "green")
                                                                                       (data-literal "yellow")))
                                                                           (list (data-id 'string))))))
  (check-exn #px"missing.*signature.*next-tl" (lambda ()
                                                (lookup-polysig sample-datadefs sample-polysigs
                                                                (polysig 'next-tl (set)
                                                                         (list (list (data-literal "red")
                                                                                     (data-literal "green")
                                                                                     (data-literal "yellow")))
                                                                         (list (data-id 'string))))))
  (check-not-exn (lambda ()
                   (lookup-polysig sample-datadefs sample-polysigs
                                   (polysig 'launch-rocket (set)
                                            (list (list (data-id 'time)))
                                            (list (data-id 'image))))))
  (check-not-exn (lambda ()
                   (lookup-polysig sample-datadefs sample-polysigs
                                   (polysig 'launch-rocket (set)
                                            (list (list (data-id 'frame)))
                                            (list (data-id 'image))))))
  (check-equal?
   (let ([lines (list ";An Invader is a Posn"
                      ";draw-invaders: [ListOf Invader] -> Image")])
     (lookup-polysig (parse-datadefs lines base-eval)
                     (parse-polysigs lines base-eval)
                     'draw-invaders))
   (parse-polysig "draw-invaders : [ListOf Posn] -> Image"
                  base-eval)))
