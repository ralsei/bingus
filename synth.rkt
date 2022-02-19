#lang racket
(require
 racket/hash
 zippers

 "ast.rkt"
 "data-definition.rkt"
 "init-environment.rkt"
 "queue.rkt"
 "satisfies.rkt"
 "unparse.rkt"
 "util.rkt")

;;;; PARTIAL PROGRAMS
;; a partial program is:
;; - a zipped expression (possibly with a hole)
;; - a compile-time environment (a hash table of terms to types, no values)
;; - a list of hole types
;; -- this can change, since the hole changes
;; -- if there are multiple holes, we fill the leftmost hole, then move right
;; a complete program is a partial program with no holes
(struct partial-program (expr cenv type) #:transparent)

;; an refinement is a pair of functions:
;; - partial-program? -> partial-program?
;;   that applies an action to a partial program
;; - partial-program? -> boolean?
;;   that determines if the action can be applied
;;
;; we don't check if it can be applied before applying it
;; (for performance reasons)
(struct program-refinement (refine possible?)
  #:property prop:procedure (struct-field-index refine)
  #:transparent)

(define (can-refine? refinement partial-prog)
  ((program-refinement-possible? refinement) partial-prog))

;; if we have a function type, introduce binders
;; function$ → lambda^
;;
;; (this is probably what the Myth paper means by η-long form,
;;  since we always do this)
(define (refine/introduce-lambda rsystem)
  (define (introduce-lambda partial-prog)
    (match-define (partial-program zipped-expr cenv (cons (function$ ins out) tys))
      partial-prog)

    (define args (map (thunk* (gensym)) ins))

    (define with-binders
      (hash-union (for/hash ([arg (in-list args)]
                             [ty (in-list ins)])
                    (values arg ty))
                  cenv))

    (define with-struct-accessors
      (apply hash-union with-binders
             (for/list ([arg (in-list args)]
                        [ty (in-list ins)]
                        #:do [(define decl
                                (and (string? ty)
                                     (hash-ref rsystem ty)))]
                        #:when (product$? decl))
               (generate-product-environment ty rsystem arg))))

    (partial-program
     (first-hole/ast (plug/ast (lambda^ args (hole^ #t)) zipped-expr))
     with-struct-accessors
     (cons out tys)))

  (define (can-introduce-lambda? partial-prog)
    (match partial-prog
      [(partial-program (zipper expr _) _ (cons (function$ _ _) _))
       (hole^? expr)]
      [_ #f]))

  (program-refinement introduce-lambda can-introduce-lambda?))

(define (refine/guess-var v)
  (define (guess-var partial-prog)
    (match-define (partial-program zipped-expr cenv tys)
      partial-prog)
    (define new-tys (rest tys))
    (partial-program
     ((cond [(empty? new-tys) identity]
            [else next-hole/ast])
      (plug/ast v zipped-expr))
     cenv
     new-tys))

  (define (can-guess-var? partial-prog)
    (match-define (partial-program (zipper focus _) cenv tys) partial-prog)
    (and (hole^? focus)
         (not (empty? tys))
         (equal? (hash-ref cenv v #f) (first tys))))

  (program-refinement guess-var can-guess-var?))

(define (refine/guess-const c)
  (define (guess-const partial-prog)
    (match-define (partial-program zipped-expr cenv tys)
      partial-prog)
    (define new-tys (rest tys))
    (partial-program
     ((cond [(empty? new-tys) identity]
            [else next-hole/ast])
      (plug/ast c zipped-expr))
     cenv
     new-tys))

  (define (can-guess-const? partial-prog)
    (match-define (partial-program (zipper focus _) _ tys) partial-prog)
    (and (hole^? focus)
         (hole^-can-fill-const? focus)
         (not (empty? tys))
         (match (first tys)
           [(number-atom$) (number? c)]
           [(string-atom$) (string? c)]
           [(boolean-atom$) (boolean? c)]
           [_ #f])))

  (program-refinement guess-const can-guess-const?))

(define (refine/guess-app fn)
  (define (guess-app partial-prog)
    (match-define (partial-program zipped-expr cenv (cons ty tys))
      partial-prog)
    (match-define (function$ ins _) (hash-ref cenv fn))

    (define args
      (cons (hole^ #f)
            (map (const (hole^ #t)) (rest ins))))

    (partial-program
     (first-hole/ast
      (plug/ast (app^ fn args) zipped-expr))
     cenv
     (append ins tys)))

  (define (can-guess-app? partial-prog)
    (match-define (partial-program (zipper focus _) cenv tys) partial-prog)
    (and (hole^? focus)
         (not (empty? tys))
         (let ([ty (hash-ref cenv fn #f)])
           (and (function$? ty)
                (equal? (function$-output ty) (first tys))))))

  (program-refinement guess-app can-guess-app?))

(define (refine/guess-template sig rsystem var-name)
  (define (guess-template partial-prog)
    (match-define (partial-program zipped-expr cenv (cons ty tys))
      partial-prog)
    (match-define (sum$ _ cases) (hash-ref rsystem sig))

    (partial-program
     (first-hole/ast
      (plug/ast (generate-sum-template sig rsystem var-name) zipped-expr))
     cenv
     (append (make-list (length cases) ty)
             tys)))

  (define (can-guess-template? partial-prog)
    ; we can actually *always* guess a template, so long as we have something
    ; that works
    (match-define (partial-program (zipper focus _) cenv tys) partial-prog)
    (and (hole^? focus)
         (not (empty? tys))
         ; products are handled by introduce-lambda,
         ; where they're added to the environment as variables
         (sum$? (hash-ref rsystem (hash-ref cenv var-name) #f))))

  (program-refinement guess-template can-guess-template?))

(define (extract-constants checks)
  (define (extract-from-quoted exp)
    (match exp
      ['() '()]
      [(cons x ys)
       #:when (or (number? x)
                  (string? x)
                  (boolean? x))
       (cons x (extract-from-quoted ys))]
      [(cons xs ys)
       #:when (list? xs)
       (append (extract-from-quoted xs)
               (extract-from-quoted ys))]
      [(cons _ ys) (extract-from-quoted ys)]
      [_ (list exp)]))

  (for/fold ([consts '()])
            ([check (in-list checks)])
    (append (extract-from-quoted (check^-actual check))
            (extract-from-quoted (check^-expected check))
            consts)))

;; XXX: there should be some kind of weighting here
(define (possible-refinements partial-prog rsystem checks)
  (match-define (partial-program _ cenv _) partial-prog)

  (define possible
    (append (list (refine/introduce-lambda rsystem))
            (for/list ([(var ty) (in-hash cenv)]
                       #:when (not (function$? ty)))
              (refine/guess-var var))
            (for/list ([atom (in-list (extract-constants checks))])
              (refine/guess-const atom))
            (for/list ([(var ty) (in-hash cenv)]
                       #:when (function$? ty))
              (refine/guess-app var))
            (for/list ([(var ty) (in-hash cenv)])
              (refine/guess-template ty rsystem var))))

  (for/list ([movement (in-list possible)]
             #:when (can-refine? movement partial-prog))
    movement))

(define (run-synth init-ty system checks)
  (define rsystem (resolve-system system))

  (define (do-bfs q)
    (cond [(queue-empty? q) #f]
          [else
           (define-values (prog others) (dequeue q))

           (match-define (partial-program zipped-expr _ tys) prog)
           (writeln (unparse (rebuild zipped-expr)))
           (define adj (possible-refinements prog rsystem checks))
           (cond [(empty? tys)
                  (define expr (rebuild zipped-expr))
                  (cond [(satisfies? (unparse-system system)
                                     (unparse expr)
                                     checks)
                         expr]
                        [else (do-bfs others)])]
                 [(empty? adj) (do-bfs others)]
                 [else
                  (define next-layer
                    (for/fold ([new-queue others])
                              ([movement (in-list adj)])
                      (enqueue (movement prog) new-queue)))
                  (do-bfs next-layer)])]))

  (do-bfs
   (enqueue
    (partial-program (zip (hole^ #f)) init-bsl-environment (list init-ty))
    empty-queue)))

(define tl-system
  (list
   (sum$ "TrafficLight"
         (list (sum-case$ (singleton-atom$ "red"))
               (sum-case$ (singleton-atom$ "yellow"))
               (sum-case$ (singleton-atom$ "green"))))))
(pretty-print
 (unparse
  (run-synth
   ; TrafficLight -> String
   (function$ (list "TrafficLight") (string-atom$))
   tl-system
   (list
    (check^ '(func "red") "no don't")
    (check^ '(func "yellow") "if you're brave")
    (check^ '(func "green") "go ahead")))))

;; this absolutely definitely does NOT work lmao
#;(define indiana-system
  (list
   (product$ "Address"
             (list (product-field$ "street" (string-atom$))
                   (product-field$ "apartment" (number-atom$))
                   (product-field$ "city" (string-atom$))
                   (product-field$ "zip" (number-atom$))))))
#;(pretty-print
 (unparse
  (run-synth
   ; Address -> Boolean
   (function$ (list "Address") (boolean-atom$))
   indiana-system
   (list
    (check^ '(func (make-address "700 N Woodlawn Ave"
                                 2062
                                 "Bloomington"
                                 47408))
            #true)
    (check^ '(func (make-address "831 E 3rd St"
                                 104
                                 "Absolutely, Totally, Not Bloomington"
                                 47405))
            #true)
    (check^ '(func (make-address "The Edge Of Indiana"
                                 0
                                 "Gary"
                                 46000))
            #true)
    (check^ '(func (make-address "The Other Edge Of Indiana"
                                 100
                                 "Evansville"
                                 47999))
            #true)
    (check^ '(func (make-address "333 East Scrimblo Lane"
                                 2062
                                 "Poggersdorf"
                                 19482))
            #false)
    (check^ '(func (make-address "444 East Crumbus Ave"
                                 0
                                 "Fucking"
                                 99999))
            #false)))))
