#lang typed/racket
(require "../common.rkt"
         "../ast-utils.rkt"
         "types.rkt"
         "resolver.rkt")
(require racket/hash)

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
#|
(struct Type-Scope ((vars : (HashTable Symbol Type))
                    (type-vars : (HashTable Symbol Type))
                    (funs : (HashTable Symbol TFunction))) #:prefab)
|#
(struct Type-Scope ((vars : Type-Map)
                    (type-vars : Type-Map)
                    (funs : (HashTable Symbol TFunction))) #:prefab)

(: empty-ts Type-Scope)
(define empty-ts (Type-Scope (hash) (hash) (hash)))

; Return union of two type scopes
(: ts-union (-> Type-Scope Type-Scope Type-Scope))
(define (ts-union x y)
  (Type-Scope
    (hash-union
      (Type-Scope-vars x)
      (Type-Scope-vars y))
    (hash-union
      (Type-Scope-type-vars x)
      (Type-Scope-type-vars y))
    (hash-union
      (make-immutable-hash (hash->list (Type-Scope-funs x)))
      (make-immutable-hash (hash->list (Type-Scope-funs y))))))

(: apply-facts (-> Type-Scope Type-Facts Type-Scope))
(define (apply-facts ts tf)
  (foldl
   (lambda ((kv : (Pair Symbol Type))
            (rst : Type-Scope))
     (define corresponding (hash-ref (Type-Scope-vars ts) (car kv) #f))
     (if corresponding
         (bind-var rst
                   (car kv)
                   (if (subtype-of? (cdr kv) corresponding) (cdr kv) corresponding))
         rst))
   ts
   (hash->list tf)))

(: bind-var (-> Type-Scope Symbol Type Type-Scope))
(define (bind-var ts var-name var-type)
  (match ts
    [(Type-Scope vars type-vars funs)
     (Type-Scope (hash-set vars var-name var-type) type-vars funs)]))

(: bind-fun (-> Type-Scope Symbol TFunction Type-Scope))
(define (bind-fun ts fun-name fun-type)
  (match ts
    [(Type-Scope vars type-vars funs)
     (Type-Scope vars type-vars (hash-set funs fun-name fun-type))]))

(: bind-type-var (-> Type-Scope Symbol Type Type-Scope))
(define (bind-type-var ts var-name var-type)
  (match ts
    [(Type-Scope vars type-vars funs)
     (Type-Scope vars (hash-set type-vars var-name var-type) funs)]))

(: lookup-var (-> Type-Scope Symbol Type))
(define (lookup-var ts var-name)
  (or (hash-ref (Type-Scope-vars ts) var-name #f)
      (context-error "undefined variable ~v"
                     (symbol->string var-name))))

(: lookup-type-var (-> Type-Scope Symbol Type))
(define (lookup-type-var ts var-name)
  (or (hash-ref (Type-Scope-type-vars ts) var-name #f)
      (context-error "undefined type ~v"
                     (symbol->string var-name))))

(: lookup-fun (-> Type-Scope Symbol TFunction))
(define (lookup-fun ts var-name)
  (or (hash-ref (Type-Scope-funs ts) var-name #f)
      (context-error "undefined function ~v"
                     (symbol->string var-name))))

(: resolve-type (-> Type-Expr Type-Map (Option Type)))
(define (resolve-type texpr env)
  (match texpr
    [`(@type-var Any) (TAny)]
    [`(@type-var Bin) (TBin)]
    [`(@type-var Nat) (TNat)]
    ;[`(@type-var ,var) (lookup-type-var 
    ;[`(@type-var ,var) (context-error "cannot resolve type names yet")]
    ;[`(@type-vec ,vec) (TVector (map (lambda (x) (resolve-type x env)) vec))]
    [`(@type-vec ,vec)
      (let ([type-vec (resolve-type* vec env)])
        (match type-vec
          [(? list? l) (TVector l)]
          [#f #f]))]
    [`(@type-vecof ,var ,count)
      (match (resolve-type var env)
        [(? Type? t) (TVectorof t count)]
        [#f #f])]
    [`(@type-bytes ,count) (TBytes count)]
    [`(@type-union ,x ,y)
      (let ([tx (resolve-type x env)]
            [ty (resolve-type y env)])
        (if (and tx ty)
          (TUnion tx ty)
          #f))]
    [_ #f]
    ))

(: resolve-type* (-> (Listof Type-Expr) Type-Map (Option (Listof Type))))
(define (resolve-type* l env)
  (foldl
    (lambda ((o : Type-Expr) (acc : (Option (Listof Type))))
      (match acc
        [#f #f]
        [(? list? l)
          (match (resolve-type o env)
            [(? Type? t) (cons t acc)]
            [#f #f])]))
    (list)
    l))

; Simply throw with a type error if option is false
(: resolve-type-or-err (-> Type-Expr Type-Map Type))
(define (resolve-type-or-err texpr env)
  (match (resolve-type texpr env)
    [(? Type? t) t]
    [_ (context-error "Failed to resolve type ~a")]))

; Read a Definition ast node and if a struct definition,
; add to the given type map. The given type map is also
; used to potentitally resolve type variables in the struct.
(: add-struct-def (-> Definition Type-Map Type-Map))
(define (add-struct-def def env)
  (match def
    [`(@def-struct ,name ,fields)
      (hash-set
        env
        name
        (TTagged
          name
          (map (lambda ([x : (List Symbol Type-Expr)])
                 (resolve-type-or-err (cadr x) env))
               fields)))]
    [_ (make-immutable-hash '())]))

; takes a Type-Scope rather than just one map because
; types may need to be resolved and the function-scope
; should also be added to.
(: add-fun-def (-> Definition Type-Scope Type-Scope))
(define (add-fun-def def accum)
  (match def
    [`(@def-fun ,fun ,args ,rettype ,expr)
      (define types-map (Type-Scope-type-vars accum))
     (define inner-scope
       (foldl (λ ((pair : (List Symbol Type-Expr)) (accum : Type-Scope))
                (bind-var
                  accum
                  (first pair)
                  (resolve-type-or-err (second pair) types-map)))
              accum
              args))
     (define inner-type (first (@-ast->type/inner expr inner-scope)))
     (: return-type Type)
     (define return-type
       (cond
         [rettype (unless (subtype-of? inner-type (resolve-type-or-err rettype types-map))
                    (context-error
                     "~a expected to return type ~a, got type ~a"
                     fun
                     (resolve-type-or-err rettype types-map)
                     inner-type))
                  (resolve-type-or-err rettype types-map)]
         [else inner-type]))
     (bind-fun accum fun
               (TFunction (map (lambda ((x : Type-Expr))
                                 (resolve-type-or-err
                                   x
                                   (Type-Scope-type-vars accum)))
                               (map (inst second Symbol Type-Expr Type-Expr)
                                    args))
                          return-type))]
    [_ empty-ts]))

(: add-def-var (-> Definition Type-Scope Type-Scope))
(define (add-def-var def accum)
  (match def
    [`(@def-var ,var ,expr)
      (bind-var accum var (first (@-ast->type/inner expr accum)))]
    [_ empty-ts]))

(: definitions->scope (-> (Listof Definition) Type-Scope))
(define (definitions->scope defs)
  (let ([struct-defs (foldl add-struct-def (Type-Scope-type-vars empty-ts) defs)]
        [var-defs (foldl add-def-var empty-ts defs)]
        ;[alias-defs (foldl add-alias-def defs)]
        [fun-defs (foldl add-fun-def empty-ts defs)])
    (foldl
      (lambda ((x : (Pairof Symbol Type)) (scope : Type-Scope))
        (bind-type-var scope (car x) (cdr x)))
      (ts-union fun-defs var-defs)
      (hash->list struct-defs))))


;; typechecks an @-ast
(: @-ast->type (-> @-Ast Type))
(define (@-ast->type ast)
  (first (@-ast->type/inner ast empty-ts)))

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

(define-type Type-Facts (Immutable-HashTable Symbol Type))

;(: @-ast->type/inner (-> @-Ast Type-Scope (List (Option Type) Type-Facts)))
(: @-ast->type/inner (-> @-Ast Type-Scope (List Type Type-Facts)))
(define (@-ast->type/inner @-ast type-scope)
  (: assert-type (-> @-Ast Type Void))
  (define (assert-type @-ast type)
    (parameterize ([current-context (context-of @-ast)])
      (define real-type (first (@-ast->type/inner @-ast type-scope)))
      (unless (subtype-of? real-type type)
        (context-error
         "expected type ~a, got type ~a"
         (type->string type)
         (type->string real-type)))))
  (define types-map (Type-Scope-type-vars type-scope))
  (: retval (List Type Type-Facts))
  (define retval
    (parameterize ([current-context (context-of @-ast)])
      (match (dectx @-ast)
        [`(@eq ,x ,y) (@-ast->type/inner x type-scope)
                      (@-ast->type/inner y type-scope)
                      (list (TBin) (hash))]
        [`(@extern ,_) (list (TAny) (hash))]
        [`(@loop ,_ ,body) (@-ast->type/inner body type-scope)]
        [`(@block ,body)
         (car (reverse (map (λ((x : @-Ast)) (@-ast->type/inner x type-scope)) body)))]
        [`(@program ,definitions ,body)
         (define new-mapping (definitions->scope definitions))
         (@-ast->type/inner body new-mapping)]
        [`(,(? (lambda(op) (member op '(@+ @- @* @/)))) ,x ,y)
         (assert-type x (TNat))
         (assert-type y (TNat))
         (list (TNat) (hash))]
        [`(@and ,x ,y)
         (match-define (list x-type x-facts) (@-ast->type/inner x type-scope))
         (match-define (list y-type y-facts)
           (@-ast->type/inner
            y
            (foldl (λ((var : (Pair Symbol Type))
                      (ts : Type-Scope))
                     (bind-var ts (car var) (cdr var)))
                   type-scope
                   (hash->list x-facts))))
         (list (smart-union x-type y-type)
               (hash-union x-facts y-facts))]
        [`(@append ,x ,y)
         ; no type facts can possibly propagate out of an @append
         (list
          (tappend (first (@-ast->type/inner x type-scope))
                   (first (@-ast->type/inner y type-scope)))
          (hash))]
        [`(@let (,val ,expr) ,body)
         (define expr-type (first (@-ast->type/inner expr type-scope)))
         (match-define (list inner-type inner-facts) (@-ast->type/inner body (bind-var type-scope val expr-type)))
         ; facts about val cannot propagate out
         (list inner-type (hash-remove inner-facts val))]
        [`(@set! ,var ,val)
         (define inner-type (first (@-ast->type/inner val type-scope)))
         (unless (subtype-of? inner-type
                              (lookup-var type-scope var))
           (context-error "cannot assign value of type ~a to variable of type ~a"
                          (type->string inner-type)
                          (type->string (lookup-var type-scope var))))
         (list (lookup-var type-scope var)
               (hash))]
        [`(@unsafe-cast ,inner ,type)
         (match-define (list actual-inner-type inner-facts) (@-ast->type/inner inner type-scope))
         (define new-type (resolve-type-or-err type types-map))
         (unless (subtype-of? new-type actual-inner-type)
         ;(unless (map (lambda (t) (subtype-of? t actual-inner-type)) new-type)
           (context-error "cannot downcast ~a to ~a"
                          (type->string actual-inner-type)
                          (type->string new-type)))
         (list new-type inner-facts)]
        [`(@ann ,inner ,type)
         (match-define (list actual-inner-type inner-facts) (@-ast->type/inner inner type-scope))
         (define new-type (resolve-type-or-err type types-map))
         (unless (subtype-of? actual-inner-type new-type)
           (context-error "cannot annotate ~a as incompatible type ~a"
                          (type->string actual-inner-type)
                          (type->string new-type)))
         (list new-type inner-facts)]
        [`(@lit-num ,n) (list (cond
                                [(= n 0) (TBin)]
                                [(= n 1) (TBin)]
                                [else (TNat)])
                              (hash))]
        [`(@for ,expr ,var ,vec)
         (let ([len (match (first (@-ast->type/inner vec type-scope))
                      [(TVectorof _ count) count]
                      [(TVector v) (length v)]
                      [(var t) (context-error "vector comprehension needs to iterate
                                              over a vector, but a ~a was
                                              provided"
                                              (type->string t))])])
           (list (TVectorof (first (@-ast->type/inner expr (bind-var type-scope var (TNat)))) len) (hash)))]
        [`(@var ,variable) (list (lookup-var type-scope variable) (hash))]
        [`(@lit-vec ,vars) (list (TVector (map (λ ((x : @-Ast)) (first (@-ast->type/inner x type-scope))) vars))
                                 (hash))]
        [`(@lit-bytes ,bts) (list (TBytes (bytes-length bts)) (hash))]
        [`(@index ,val ,idx-expr)
         (: idx Integer)
         (define idx
           (match (dectx idx-expr)
             [`(@lit-num ,x) x]
             [other (context-error "non-literal index ~a not yet supported" other)]))
         (list
          (match (first (@-ast->type/inner val type-scope))
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
           [other (context-error "cannot index into a value of type ~a" other)])
          (hash))]
        [`(@apply itob ,args)
         (unless (= 1 (length args))
           (context-error "itob must only take 1 argument"))
         (assert-type (car args) (TNat))
         (list (TBytes 32) (hash))]
        [`(@apply btoi ,args)
         (unless (= 1 (length args))
           (context-error "itob must only take 1 argument"))
         (assert-type (car args) (TBytes 32))
         (list (TNat) (hash))]
        [`(@apply ,fun ,args)
         (match (lookup-fun type-scope fun)
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
            (list result (hash))]
           [_ (error '@-ast->type "[~a] undefined function ~a"
                     (context->string (get-context @-ast)) fun)])]
        [`(@is ,expr ,type)
         (list (TBin)
               (match (dectx expr)
                 [`(@var ,var)
                   (make-immutable-hash `((,var . ,(resolve-type-or-err type types-map))))]
                 [else (hash)]))]
        [`(@if ,cond ,happy ,sad)
         (match-define (list _ facts) (@-ast->type/inner cond type-scope)) ; just to check
         (list
          (smart-union (first (@-ast->type/inner happy (apply-facts type-scope facts)))
                       (first (@-ast->type/inner sad type-scope)))
          (hash))]
        )))
  (hash-set! TYPE-MEMOIZER @-ast (first retval))
  retval)

(module+ test
  (require "../parser.rkt")
  (parameterize ([FILENAME "test.melo"])
    (@-ast->type/inner
     (melo-parse-port (open-input-string "
let x = ann 1 : Any in
if x is Nat && x == 10 then x*x else 123
"))
     empty-ts)))