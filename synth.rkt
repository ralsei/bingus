#lang racket
(require
 racket/hash
 zippers

 "ast.rkt"
 "bsl-require.rkt"
 "init-environment.rkt"
 "queue.rkt"
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
      (match-define (partial-program zipped-expr cenv (cons (function$ ins out) tys))
        partial-prog)

      (define args (map (thunk* (gensym)) ins))
      (define new-cenv
        (hash-union (for/hash ([arg (in-list args)]
                               [ty (in-list ins)])
                      (values arg ty))
                    cenv))
      (partial-program
       (first-hole/ast (plug/ast (lambda^ args (hole^)) zipped-expr))
       new-cenv
       (cons out tys)))

    (define (can-introduce-lambda? partial-prog)
      (match partial-prog
        [(partial-program (zipper expr _) _ (cons (function$ _ _) _))
         (hole^? expr)]
        [_ #f]))

    (program-refinement introduce-lambda can-introduce-lambda?)))

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
    (partial-program
     (first-hole/ast
      (plug/ast (app^ fn (map (const (hole^)) ins)) zipped-expr))
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

;; these are traversed in order.
;; if we want to avoid going down an infinite rabbit hole,
;; we should introduce constants here
;;
;; that, or search the tree in BFS, not DFS
(define (possible-refinements partial-prog checks)
  (match-define (partial-program _ cenv _) partial-prog)

  (define possible
    (append (list refine/introduce-lambda)
            (for/list ([(var ty) (in-hash cenv)]
                       #:when (not (function$? ty)))
              (refine/guess-var var))
            (for/list ([atom (in-list (extract-constants checks))])
              (refine/guess-const atom))
            (for/list ([(var ty) (in-hash cenv)]
                       #:when (function$? ty))
              (refine/guess-app var))))

  (for/list ([movement (in-list possible)]
             #:when (can-refine? movement partial-prog))
    movement))

(define (run-synth init-ty checks)
  (define (do-bfs q)
    (cond [(queue-empty? q) #f]
          [else
           (define-values (prog others) (dequeue q))

           (match-define (partial-program zipped-expr _ tys) prog)
           (define adj (possible-refinements prog checks))
           (cond [(empty? tys)
                  (define expr (rebuild zipped-expr))
                  (cond [(satisfies? (list (unparse expr)) checks) expr]
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
    (partial-program (zip (hole^)) init-bsl-environment (list init-ty))
    empty-queue)))
