#lang racket
(require racket/hash
         zippers

         "ast.rkt"
         "data-definition.rkt"
         "init-environment.rkt"
         "queue.rkt"
         "satisfies.rkt"
         "unparse.rkt"
         "util.rkt")
(provide run-synth)

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
(define refine/introduce-lambda
  (let ()
    (define (introduce-lambda partial-prog)
      (match-define (zipper (hole^ _ cenv (function$ ins out _) checks) _)
        partial-prog)

      (define args
        (cond [(current-function-arguments) => identity]
              [else (map (thunk* (gensym)) ins)]))

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
                                       (hash-ref (current-resolved-system) ty)))]
                          #:when (product$? decl))
                 (generate-product-environment decl
                                               #:var-name arg
                                               #:cenv cenv
                                               #:checks checks))))

      (first-hole/ast
       (plug/ast (lambda^ args (hole^ #t with-struct-accessors out checks))
                 partial-prog)))

    (define (can-introduce-lambda? partial-prog)
      (match partial-prog
        [(zipper (hole^ _ _ (function$ _ _ _) _) _) #t]
        [_ #f]))

    (program-refinement introduce-lambda can-introduce-lambda?)))

(define (refine/guess-var v)
  (define (guess-var partial-prog)
    ((cond [(complete? v) next-hole/ast]
           [else first-hole/ast])
     (plug/ast v partial-prog)))

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
    (match-define (function$ ins _ constructor?) (hash-ref cenv fn))

    (define args
      (cond ;; at least one argument should be a non-constant
            ;; (ex. (+ 3 5) is useless)
            ;; unless it's a constructor
            [constructor?
             (map (λ (in) (hole^ #t cenv in checks)) ins)]
            ;; BSL doesn't allow nullary functions, so we don't need to check empty
            [else
             (cons (hole^ #f cenv (first ins) checks)
                   (map (λ (in) (hole^ #t cenv in checks)) (rest ins)))]))

    (first-hole/ast (plug/ast (app^ fn args) partial-prog)))

  (define (can-guess-app? partial-prog)
    (match-define (zipper focus _) partial-prog)
    (and (hole^? focus)
         (let ([sig (hash-ref (hole^-cenv focus) fn #f)])
           (and (function$? sig)
                (equal? (function$-output sig) (hole^-signature focus))))))

  (program-refinement guess-app can-guess-app?))

;; HACK: make this non-global after demos -- but boy does it make things fast
(define SPLITS (mutable-set))

(define (refine/guess-template sum var-name)
  (define (guess-template partial-prog)
    (match-define (zipper (hole^ _ cenv sig checks) _)
      partial-prog)

    (set-add! SPLITS var-name)

    (first-hole/ast
     (plug/ast (generate-sum-template sum
                                      #:var-name var-name
                                      #:cenv cenv
                                      #:signature sig
                                      #:checks checks)
               partial-prog)))

  (define (can-guess-template? partial-prog)
    ; we can actually *always* guess a template, so long as we have something
    ; that works
    (match-define (zipper focus _) partial-prog)
    (and (hole^? focus)
         (not (app^? var-name))
         (not (set-member? SPLITS var-name))
         ; products are handled by introduce-lambda,
         ; where they're added to the environment as variables
         (sum$? (hash-ref (current-resolved-system)
                          (hash-ref (hole^-cenv focus) var-name)
                          #f))))

  (program-refinement guess-template can-guess-template?))

(define (extract-constants checks)
  (define (extract-from-quoted exp)
    (match exp
      ['() (set)]
      [(cons x ys)
       #:when (or (number? x)
                  (string? x)
                  (boolean? x))
       (set-add (extract-from-quoted ys) x)]
      [(cons xs ys)
       #:when (list? xs)
       (set-union (extract-from-quoted xs)
                  (extract-from-quoted ys))]
      [(cons _ ys) (extract-from-quoted ys)]
      [_ (set exp)]))

  (for/fold ([consts (set)])
            ([check (in-list checks)])
    (set-union consts
               (extract-from-quoted (check^-actual check))
               (extract-from-quoted (check^-expected check)))))

;; XXX: there should be some kind of weighting here
(define (possible-refinements partial-prog)
  (match-define (zipper (hole^ _ cenv _ checks) _) partial-prog)

  (define possible
    (append (list refine/introduce-lambda)
            (for/list ([(var ty) (in-hash cenv)]
                       #:when (not (function$? ty)))
              (refine/guess-var var))
            (for/list ([atom (in-set (extract-constants checks))])
              (refine/guess-const atom))
            (for/list ([(var ty) (in-hash cenv)]
                       #:when (function$? ty))
              (refine/guess-app var))
            (for/list ([(var ty) (in-hash cenv)])
              (refine/guess-template ty var))))
  
  (for/list ([movement (in-list possible)]
             #:when (can-refine? movement partial-prog))
    movement))

(define (run-synth function-name init-ty system checks
                   #:debug? [debug? #f]
                   #:args [args #f])
  (set-clear! SPLITS)

  (parameterize ([current-resolved-system (resolve-system system)]
                 [current-function-name function-name]
                 [current-function-type init-ty]
                 [current-function-arguments args])
    (define (do-bfs q)
      (cond [(queue-empty? q) #f]
            [else
             (define-values (prog others) (dequeue q))

             (match-define (zipper focus _) prog)
             (when debug?
               (pretty-write (unparse (rebuild prog))))
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
                                ([movement (in-list (possible-refinements prog))])
                        (enqueue (movement prog) new-queue)))
                    (cond [(empty? next-layer) (do-bfs others)]
                          [else (do-bfs next-layer)])])]))

    (unparse
     (do-bfs (enqueue
              (zip (hole^ #f
                          (hash-union init-bsl-environment
                                      (system->environment system))
                          init-ty
                          checks))
              empty-queue)))))
