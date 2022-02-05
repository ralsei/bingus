#lang racket
(require
 racket/hash
 zippers

 "ast.rkt"
 "bsl-require.rkt"
 "unparse.rkt"
 "util.rkt")

;;;; PARTIAL PROGRAMS
;; a partial program is:
;; - a zipped expression (possibly with a hole)
;; - a compile-time environment (a hash table of terms to types, no values)
;; - a hole type
;; -- this can change, since the hole changes
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

(define (satisfies? prog checks)
  (for/and ([check (in-list checks)])
    (match-define (check^ actual expected) check)
    (equal? (eval/bsl prog actual)
            (eval/bsl prog expected))))

;; if we have a function type, introduce binders
;; function$ → lambda^
;;
;; (this is probably what the Myth paper means by η-long form,
;;  since we always do this)
(define refine/introduce-lambda
  (let ()
    (define (introduce-lambda partial-prog)
      (match-define (partial-program zipped-expr cenv (function$ ins out))
        partial-prog)

      (define args (map (thunk* (gensym)) ins))
      (define new-cenv
        (hash-union (for/hash ([arg (in-list args)]
                               [ty (in-list ins)])
                      (values arg ty))
                    cenv))
      (partial-program
       (down/lambda^-body
        (edit (const (lambda^ args (hole^)))
              zipped-expr))
       new-cenv
       out))

    (define (can-introduce-lambda? partial-prog)
      (match partial-prog
        [(partial-program (zipper expr _) _ (function$ _ _))
         (hole^? expr)]
        [_ #f]))

    (program-refinement introduce-lambda can-introduce-lambda?)))

(define (refine/guess-var v)
  (define (guess-var partial-prog)
    (match-define (partial-program zipped-expr cenv _)
      partial-prog)
    (partial-program (edit (const v) zipped-expr)
                     cenv
                     (no-goal$)))

  (define (can-guess-var? partial-prog)
    (match-define (partial-program (zipper focus _) cenv ty) partial-prog)
    (and (hole^? focus)
         (equal? (hash-ref cenv v #f) ty)))

  (program-refinement guess-var can-guess-var?))

(define (refine/guess-const c)
  (define (guess-const partial-prog)
    (match-define (partial-program zipped-expr cenv _)
      partial-prog)
    (partial-program (edit (const c) zipped-expr)
                     cenv
                     (no-goal$)))

  (define (can-guess-const? partial-prog)
    (match-define (partial-program (zipper focus _) _ ty) partial-prog)
    (and (hole^? focus)
         (match ty
           [(number-atom$) (number? c)]
           [(string-atom$) (string? c)]
           [_ #f])))

  (program-refinement guess-const can-guess-const?))

(define (extract-constants checks)
  ;; for now, just take the examples
  (for/list ([check (in-list checks)])
    (check^-expected check)))

;; these are traversed in order.
;; if we want to avoid going down an infinite rabbit hole,
;; we should introduce constants here
;;
;; that, or search the tree in BFS, not DFS
(define (possible-refinements partial-prog checks)
  (match-define (partial-program _ cenv _) partial-prog)

  (define possible
    (append (list refine/introduce-lambda)
            (for/list ([(var _) (in-hash cenv)])
              (refine/guess-var var))
            (for/list ([atom (in-list (extract-constants checks))])
              (refine/guess-const atom))))

  (for/list ([movement (in-list possible)]
             #:when (can-refine? movement partial-prog))
    movement))

(define (run-synth init-ty checks)
  (define (do-dfs partial-prog)
    (match-define (partial-program zipped-expr _ ty) partial-prog)
    (define adj (possible-refinements partial-prog checks))
    (cond [(no-goal$? ty)
           (define expr (rebuild zipped-expr))
           (and (satisfies? (list (unparse expr)) checks)
                expr)]
          [(empty? adj) #f]
          [else
           (for*/first ([movement (in-list adj)]
                        [result (in-value (do-dfs (movement partial-prog)))]
                        #:when result)
             result)]))

  (do-dfs (partial-program (zip (hole^)) (hash) init-ty)))
