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
;; a partial program is a zipper of an AST.

;; an refinement is a pair of functions:
;; - partial-program? -> partial-program?
;;   that applies an action to a partial program
;; - partial-program? -> boolean?
;;   that determines if the action can be applied
;;
;; we don't check if it can be applied before applying it,
;; that's done during the BFS.
;;
;; we terminate when we're no longer focused on a hole.
;; each refinement should jump to the next hole.
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
    (match-define (zipper (hole^ _ cenv (function$ ins out) checks) _)
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

    (first-hole/ast
     (plug/ast (lambda^ args (hole^ #t with-struct-accessors out checks))
               partial-prog)))

  (define (can-introduce-lambda? partial-prog)
    (match partial-prog
      [(zipper (hole^ _ _ (function$ _ _) _) _) #t]
      [_ #f]))

  (program-refinement introduce-lambda can-introduce-lambda?))

(define (refine/guess-var v)
  (define (guess-var partial-prog)
    (next-hole/ast (plug/ast v partial-prog)))

  (define (can-guess-var? partial-prog)
    (match-define (zipper focus _) partial-prog)
    (and (hole^? focus)
         (equal? (hash-ref (hole^-cenv focus) v #f) (hole^-signature focus))))

  (program-refinement guess-var can-guess-var?))

(define (refine/guess-const c)
  (define (guess-const partial-prog)
    (next-hole/ast (plug/ast c partial-prog)))

  (define (can-guess-const? partial-prog)
    (match-define (zipper focus _) partial-prog)
    (and (hole^? focus)
         (hole^-can-fill-const? focus)
         (match (hole^-signature focus)
           [(number-atom$) (number? c)]
           [(string-atom$) (string? c)]
           [(boolean-atom$) (boolean? c)]
           [_ #f])))

  (program-refinement guess-const can-guess-const?))

(define (refine/guess-app fn)
  (define (guess-app partial-prog)
    (match-define (zipper (hole^ _ cenv sig checks) _)
      partial-prog)
    (match-define (function$ ins _) (hash-ref cenv fn))

    (define args
      (cons (hole^ #f cenv (first ins) checks)
            (map (λ (in) (hole^ #t cenv in checks)) (rest ins))))

    (first-hole/ast (plug/ast (app^ fn args) partial-prog)))

  (define (can-guess-app? partial-prog)
    (match-define (zipper focus _) partial-prog)
    (and (hole^? focus)
         (let ([sig (hash-ref (hole^-cenv focus) fn #f)])
           (and (function$? sig)
                (equal? (function$-output sig) (hole^-signature focus))))))

  (program-refinement guess-app can-guess-app?))

(define (refine/guess-template sum rsystem var-name)
  (define (guess-template partial-prog)
    (match-define (zipper (hole^ _ cenv sig checks) _)
      partial-prog)
    (match-define (sum$ _ cases) (hash-ref rsystem sum))

    (first-hole/ast
     (plug/ast (generate-sum-template sum rsystem
                                      #:var-name var-name
                                      #:cenv cenv
                                      #:signature sig
                                      #:checks checks))))

  (define (can-guess-template? partial-prog)
    ; we can actually *always* guess a template, so long as we have something
    ; that works
    (match-define (zipper focus _) partial-prog)
    (and (hole^? focus)
         ; products are handled by introduce-lambda,
         ; where they're added to the environment as variables
         (sum$? (hash-ref rsystem (hash-ref (hole^-cenv focus) var-name) #f))))

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
(define (possible-refinements partial-prog rsystem)
  (match-define (zipper (hole^ _ cenv _ checks) _) partial-prog)

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

           (match-define (zipper focus _) prog)
           #;(writeln (unparse (rebuild prog)))
           (cond [(not (hole^? focus))
                  (define expr (rebuild prog))
                  (cond [(satisfies? (unparse-system system)
                                     (unparse expr)
                                     checks)
                         expr]
                        [else (do-bfs others)])]
                 [else
                  (define next-layer
                    (for/fold ([new-queue others])
                              ([movement (in-list (possible-refinements prog rsystem))])
                      (enqueue (movement prog) new-queue)))
                  (do-bfs (cond [(empty? next-layer) others]
                                [else next-layer]))])]))

  (do-bfs
   (enqueue
    (zip (hole^ #f
                (hash-union init-bsl-environment
                            (system->environment system))
                init-ty
                checks))
    empty-queue)))

;;; various tests
(define emp-system
  (list
   (product$ "Point"
             (list (product-field$ "x" (number-atom$))
                   (product-field$ "y" (number-atom$))))

   (product$ "None" '())
   (product$ "One"
             (list (product-field$ "first" "Point")))
   (product$ "Two"
             (list (product-field$ "first" "Point")
                   (product-field$ "second" "Point")))

   (sum$ "EvenMorePoints"
         (list (sum-case$ "None")
               (sum-case$ "One")
               (sum-case$ "Two")))))

#;(pretty-print
 (unparse
  (run-synth
   (function$ (list "Point" "EvenMorePoints") "EvenMorePoints")
   emp-system
   (list
    (check^ '(func (make-point 3 2) (make-none)) '(make-one (make-point 3 2)))
    (check^ '(func (make-point 3 2) (make-one (make-point 4 5)))
            '(make-two (make-point 4 5) (make-point 3 2)))
    (check^ '(func (make-point 9 2) (make-two (make-point 9 3) (make-point 4 2)))
            '(make-three (make-point 9 2) (make-point 9 3) (make-point 4 2)))
    (check^ '(func (make-point 0 0) (make-three (make-point 9 2) (make-point 9 3) (make-point 4 2)))
            '(make-three (make-point 0 0) (make-point 9 2) (make-point 9 3)))))))

(define bon-system
  (list
   (product$ "None" '())
   (product$ "Some"
             (list (product-field$ "first" (number-atom$))
                   (product-field$ "rest" "BunchOfNumbers")))
   (sum$ "BunchOfNumbers"
         (list (sum-case$ "None")
               (sum-case$ "Some")))))

(define tl-system
  (list
   (sum$ "TrafficLight"
         (list (sum-case$ (singleton-atom$ "red"))
               (sum-case$ (singleton-atom$ "yellow"))
               (sum-case$ (singleton-atom$ "green"))))))
#;(pretty-print
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
(define indiana-system
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
