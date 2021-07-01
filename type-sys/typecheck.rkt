#lang typed/racket
(require "../common.rkt"
         "types.rkt"
         "resolver.rkt")
(provide Type
         @-ast->type
         to-tvector
         memoized-type)


;; Converts from TVector to TVectorof
(: to-tvector (-> (U TVectorof TVector) TVector))
(define (to-tvector tvec)
  (match tvec
    [(TVectorof inner count)
     (TVector (make-list count inner))]
    [(? TVector? x) x]))

;; Appends two vectors
(: tappend (-> Type Type Type))
(define (tappend left right)
  (match (cons left right)
    [(cons (TBytes n)
           (TBytes m)) (TBytes (+ n m))]
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
     (tappend (to-tvector left)
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
    [`(@type-var Any) (TAny)]
    [`(@type-var Bin) (TBin)]
    [`(@type-var Nat) (TNat)]
    [`(@type-var ,var) (context-error "cannot resolve type names yet")]
    [`(@type-vec ,vec) (TVector (map resolve-type vec))]
    [`(@type-vecof ,var ,count) (TVectorof (resolve-type var) count)]
    [`(@type-union ,x ,y) (TUnion (resolve-type x)
                                  (resolve-type y))]
    ))

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
              (define inner-type (@-ast->type/inner expr inner-scope))
              (: return-type Type)
              (define return-type
                (cond
                  [rettype (unless (subtype-of? inner-type (resolve-type rettype))
                             (context-error
                              "~a expected to return type ~a, got type ~a"
                              fun
                              (resolve-type rettype)
                              inner-type))
                           (resolve-type rettype)]
                  [else inner-type]))
              (bind-fun accum fun
                        (TFunction (map resolve-type (map (inst second Symbol Type-Expr Any)
                                                          args))
                                   return-type))]))
         empty-ts
         defs))

;; typechecks an @-ast
(: @-ast->type (-> @-Ast Type))
(define (@-ast->type ast)
  (@-ast->type/inner ast empty-ts))

;; lookups the memoized type for the subexpression
(: memoized-type (-> @-Ast Type))
(define (memoized-type ast)
  (parameterize ([current-context (context-of ast)])
    (match (hash-ref TYPE-MEMOIZER ast #f)
      [#f (context-error "cannot lookup memoized type")]
      [(? Type? x) x])))

;; "smart union" of two types that doesn't create a TUnion if one is the subtype of another
(: smart-union (-> Type Type Type))
(define (smart-union t1 t2)
  (cond
    [(subtype-of? t1 t2) t2]
    [(subtype-of? t2 t1) t1]
    [else (TUnion t1 t2)]))

(: TYPE-MEMOIZER (HashTable @-Ast Type))
(define TYPE-MEMOIZER (make-weak-hasheq))

(: @-ast->type/inner (-> @-Ast Type-Scope Type))
(define (@-ast->type/inner @-ast scope)
  (: assert-type (-> @-Ast Type Void))
  (define (assert-type @-ast type)
    (parameterize ([current-context (context-of @-ast)])
      (define real-type (@-ast->type/inner @-ast scope))
      (unless (subtype-of? real-type type)
        (context-error
         "expected type ~a, got type ~a"
         (type->string type)
         (type->string real-type)))))
  (define retval
    (parameterize ([current-context (context-of @-ast)])
    (match (dectx @-ast)
      [`(@loop ,_ ,body) (@-ast->type/inner body scope)]
      [`(@block ,body)
       (car (reverse (map (λ((x : @-Ast)) (@-ast->type/inner x scope)) body)))]
      [`(@program ,definitions ,body)
       (define new-mapping (definitions->scope definitions))
       (@-ast->type/inner body new-mapping)]
      [`(,(? (lambda(op) (member op '(@+ @- @* @/)))) ,x ,y)
       (assert-type x (TNat))
       (assert-type y (TNat))
       (TNat)]
      [`(@append ,x ,y)
       (tappend (@-ast->type/inner x scope)
                       (@-ast->type/inner y scope))]
      [`(@let (,val ,expr) ,body)
       (define expr-type (@-ast->type/inner expr scope))
       (@-ast->type/inner body (bind-var scope val expr-type))]
      [`(@set! ,var ,val)
       (define inner-type (@-ast->type/inner val scope))
       (unless (subtype-of? inner-type
                            (lookup-var scope var))
         (context-error "cannot assign value of type ~a to variable of type ~a"
                        (type->string inner-type)
                        (type->string (lookup-var scope var))))
       (lookup-var scope var)]
      [`(@unsafe-cast ,inner ,type)
       (define actual-inner-type (@-ast->type/inner inner scope))
       (define new-type (resolve-type type))
       (unless (subtype-of? new-type actual-inner-type)
         (context-error "cannot downcast ~a to ~a"
                        (type->string actual-inner-type)
                        (type->string new-type)))
       new-type]
      [`(@ann ,inner ,type)
       (define actual-inner-type (@-ast->type/inner inner scope))
       (define new-type (resolve-type type))
       (unless (subtype-of? actual-inner-type new-type)
         (context-error "cannot annotate ~a as incompatible type ~a"
                        (type->string actual-inner-type)
                        (type->string new-type)))
       new-type]
      [`(@lit-num ,n) (cond
                        [(= n 0) (TBin)]
                        [(= n 1) (TBin)]
                        [else (TNat)])]
      [`(@for ,expr ,var ,vec)
        (let ([len (match (@-ast->type/inner vec scope)
                     [(TVectorof _ count) count]
                     [(TVector v) (length v)]
                     [(var t) (context-error "vector comprehension needs to iterate
                                              over a vector, but a ~a was
                                              provided" t)])])
          (TVectorof (@-ast->type/inner expr (bind-var scope var (TNat))) len))]
      [`(@var ,variable) (lookup-var scope variable)]
      [`(@lit-vec ,vars) (TVector (map (λ ((x : @-Ast)) (@-ast->type/inner x scope)) vars))]
      [`(@lit-bytes ,bts) (TBytes (bytes-length bts))]
      [`(@index ,val ,idx-expr)
       (: idx Integer)
       (define idx
         (match (dectx idx-expr)
           [`(@lit-num ,x) x]
           [other (context-error "non-literal index ~a not yet supported" other)]))
       (match (@-ast->type/inner val scope)
         [(TVector lst) (unless (< idx (length lst))
                           (context-error "index ~a out of bounds for vector of ~a elements"
                                          idx
                                          (length lst)))
                         (list-ref lst idx)]
         [(TVectorof t n) (unless (< idx n)
                              (context-error "index ~a out of bounds for vector of ~a elements"
                                             idx
                                             n))
                            t]
         [other (context-error "cannot index into a value of type ~a" other)])]
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
      [`(@if ,cond ,happy ,sad)
       (smart-union (@-ast->type/inner happy scope)
                    (@-ast->type/inner sad scope))]
      )))
  (hash-set! TYPE-MEMOIZER @-ast retval)
  retval)

(module+ test
  (require "../parser.rkt")
  (parameterize ([FILENAME "test.melo"])
    (type->string
     (@-ast->type/inner
      (melo-parse-port (open-input-string "
def laboo(yah : [Nat * 6]) = yah.0

let lst = laboo([1, 2, 3, 4, 5, 6]) in
let lst = [ann lst : Any] in
do
  set! lst = [[2, 3]]
  lst ++ lst
end
"))
      empty-ts))))