#lang typed/racket
(require "../common.rkt"
         "types.rkt")
(provide Type
         @-ast->type)


;; Converts from TVector to TVectorof
(: to-tvector (-> (U TVectorof TVector) TVector))
(define (to-tvector tvec)
  (match tvec
    [(TVectorof inner count)
     (TVector (make-list count inner))]
    [(? TVector? x) x]))

;; Appends two vectors
(: tvector-append (-> Type Type Type))
(define (tvector-append left right)
  (match (cons left right)
    ;; [T U ..] ++ [V W X ..]
    [(cons (TVector left-types)
           (TVector right-types)) (TVector (append left-types right-types))]
    ;; [T; n] ++ [T; m]
    [(cons (TVectorof left-type left-count)
           (TVectorof right-type right-count))
     (unless (equal? left-type right-type)
       (context-error "tried to append vectors with mismatching types: ~a ++ ~a"
                      left-type
                      right-type))
     (TVectorof left-type (+ left-count right-count))]
    ;; [T T ..] ++ [T; n]
    [(cons (? TVectorU? left)
           (? TVectorU? right))
     (tvector-append (to-tvector left)
                     (to-tvector right))]
    [_
     (context-error "cannot append non-vectors: ~a ++ ~a"
                    left
                    right)]
    ))

;; A function
(struct TFunction ((arg-types : (Listof Type))
                   (result-type : Type)))

;; A type scope
(struct Type-Scope ((vars : (HashTable Symbol Type))
                    (funs : (HashTable Symbol TFunction))) #:prefab)

(: empty-ts Type-Scope)
(define empty-ts (Type-Scope (hash) (hash)))

(: bind-var (-> Type-Scope Symbol Type Type-Scope))
(define (bind-var ts var-name var-type)
  (match ts
    [(Type-Scope vars funs)
     (Type-Scope (hash-set vars var-name var-type) funs)]))

(: bind-fun (-> Type-Scope Symbol TFunction Type-Scope))
(define (bind-fun ts fun-name fun-type)
  (match ts
    [(Type-Scope vars funs)
     (Type-Scope vars (hash-set funs fun-name fun-type))]))

(: lookup-var (-> Type-Scope Symbol Type))
(define (lookup-var ts var-name)
  (or (hash-ref (Type-Scope-vars ts) var-name #f)
      (context-error "undefined variable ~v"
                     (symbol->string var-name))))

(: lookup-fun (-> Type-Scope Symbol TFunction))
(define (lookup-fun ts var-name)
  (or (hash-ref (Type-Scope-funs ts) var-name #f)
      (context-error "undefined variable ~v"
                     (symbol->string var-name))))

(: resolve-type (-> Type-Expr Type))
(define (resolve-type texpr)
  (match texpr
    [`(@type-var Nat) (TNat)]
    [`(@type-var ,var) (context-error "cannot resolve type names yet")]
    [`(@type-vec ,vec) (TVector (map resolve-type vec))]
    [`(@type-vecof ,var ,count) (TVectorof (resolve-type var) count)]))

;; Produces an initial scope given a bunch of definitions
(: definitions->scope (-> (Listof Definition) Type-Scope))
(define (definitions->scope defs)
  (foldl (λ ((binding : Definition) (accum : Type-Scope))
           (match binding
             [`(@def-var ,var ,expr) (bind-var accum var (@-ast->type/inner expr accum))]
             [`(@def-fun ,fun ,args ,rettype ,expr)
              (define inner-scope
                (foldl (λ ((pair : (List Symbol Type-Expr)) (accum : Type-Scope))
                         (bind-var accum (first pair) (resolve-type (second pair))))
                       accum
                       args))
              (bind-fun accum fun
                        (TFunction (map resolve-type (map (inst second Symbol Type-Expr Any)
                                                          args))
                                   (@-ast->type/inner expr inner-scope)))]))
         empty-ts
         defs))

;; typechecks an @-ast
(: @-ast->type (-> @-Ast Type))
(define (@-ast->type ast)
  (@-ast->type/inner ast empty-ts))

(: @-ast->type/inner (-> @-Ast Type-Scope Type))
(define (@-ast->type/inner @-ast scope)
  
  (: assert-type (-> @-Ast Type Void))
  (define (assert-type @-ast type)
    (define real-type (@-ast->type/inner @-ast scope))
    (unless (equal? real-type type)
      (context-error
       "expected type ~a, got type ~a"
       type
       real-type)))
  (parameterize ([current-context (context-of @-ast)])
    (match (dectx @-ast)
      [`(@program ,definitions ,body)
       (define new-mapping (definitions->scope definitions))
       (@-ast->type/inner body new-mapping)]
      [`(,(? (lambda(op) (member op '(@+ @- @* @/)))) ,x ,y)
       (assert-type x (TNat))
       (assert-type y (TNat))
       (TNat)]
      [`(@append ,x ,y)
       (tvector-append (@-ast->type/inner x scope)
                       (@-ast->type/inner y scope))]
      [`(@let (,val ,expr) ,body)
       (define expr-type (@-ast->type/inner expr scope))
       (@-ast->type/inner body (bind-var scope val expr-type))]
      [`(@lit-num ,_) (TNat)]
      [`(@var ,variable) (lookup-var scope variable)]
      [`(@lit-vec ,vars) (TVector (map (λ ((x : @-Ast)) (@-ast->type/inner x scope)) vars))]
      [`(@apply ,fun ,args)
       (match (lookup-fun scope fun)
         [(TFunction arg-types result)
          (unless (equal? (length arg-types) (length args))
            (error '@-ast->type "[~a] calling function ~a with ~a arguments instead of the expected ~a"
                   (context->string (get-context @-ast))
                   fun
                   (length args)
                   (length arg-types)))
          (for ([arg-expr (in-list args)]
                [arg-type (in-list arg-types)])
            (assert-type arg-expr arg-type))
          result]
         [_ (error '@-ast->type "[~a] undefined function ~a"
                   (context->string (get-context @-ast)) fun)])]
      )))

(module+ test
  (require "../parser.rkt")
  (parameterize ([FILENAME "test.melo"])
    (@-ast->type/inner
     (melo-parse-port (open-input-string "
def app (x [Nat], y [Nat]) = x ++ y
def laboo (x [Nat 2]) = x ++ x

laboo(app([1], [2]))
"))
     empty-ts)))