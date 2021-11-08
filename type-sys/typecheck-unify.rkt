#lang typed/racket
(require "type-bag.rkt"
         "types.rkt"
         "../asts/raw-ast.rkt")
(provide type-index
         type-append
         type-cons
         type-push
         type-unify
         type-template-fill
         cg-template-fill
         vec-index-type
         subtype-of?)

; Check if t2 is a subtype of t1
(: subtype-of? (-> Type Type Boolean))
(define (subtype-of? t1 t2)
  (bag-subtype-of? (type->bag t1)
                   (type->bag t2)))

; The inner type is just the union of all
; types in the type vector
(: tvector-inner-type (-> (Listof Type) Type))
(define (tvector-inner-type v)
  ;(define v (TVector-lst tvec))
  (foldl (λ((t : Type) (acc : Type)) (TUnion t acc))
         (car v)
         (cdr v)))

; Return the index type of a vector
; useful for variable indexing where the exact location is unknown
(: vec-index-type (-> Type Type))
(define (vec-index-type t)
  (match t
    [(TVectorof inner-type _) inner-type]
    [(TDynVectorof inner-type) inner-type]
    [(TVector ts)
     (bag->type (foldl bag-union
                       empty-bag
                       (map type->bag ts)))]))

(: type-index (-> Type Integer Type))
(define (type-index type idx)
  (define bagged (type->bag type))
  ;; we first make sure the vector has the right length, because projection is infallible --- it returns a set of facts, not a type
  (for ([length-case (Type-Bag-inner (bag-project bagged `(len root)))])
    (define length (with-handlers ([exn:fail? (λ _ (context-error "type ~a has unknown length"
                                                                  (type->string type)))])
                     (cast (hash-ref length-case 'root) Integer)))
    (unless (< idx length)
      (context-error "cannot index into type ~a with index ~a because it may be of a shorter length ~a"
                     (type->string type)
                     idx
                     length)))
  ;; then we find the possible types
  (define projected (bag-project bagged `(ref root ,idx)))
  (bag->type (bag-project bagged `(ref root ,idx))))

; Take a vector/bytes type and
; push a type to it
(: type-push (-> Type Type Type))
(define (type-push t u)
  ;; convert to bags
  (define t-bag (type->bag t))
  (define u-bag (type->bag u))
  ;; cartesian-product the two bags
  (for*/fold ([accum : Type (TNone)])
             ([t-case (Type-Bag-inner t-bag)]
              [u-case (Type-Bag-inner u-bag)]) : Type
    (TUnion accum
            (match (cons (bag-case->type t-case)
                         (bag-case->type u-case))
              [(cons (TVectorof inner-u const-expr-u) t)
               (if (equal? inner-u t)
                 (TVectorof t (normal-form `(+ 1 ,const-expr-u)))
                 (context-error "Cannot push type ~a to a vector of ~a"
                                (type->string t)
                                (type->string inner-u)))]
              [(cons (TVector u-list) t)
               (TVector (append u-list (list t)))]
              [(cons t (TVector u-list))
               (TVector (append (list t) u-list))]
              [(cons rt ru) (context-error "cannot push type ~a to a ~a"
                              (type->string rt)
                              (type->string ru))]))))

; Take a type and a vector/bytes type
; and cons the type to it
(: type-cons (-> Type Type Type))
(define (type-cons t u)
  ;; convert to bags
  (define t-bag (type->bag t))
  (define u-bag (type->bag u))
  ;; cartesian-product the two bags
  (for*/fold ([accum : Type (TNone)])
             ([t-case (Type-Bag-inner t-bag)]
              [u-case (Type-Bag-inner u-bag)]) : Type
    (TUnion accum
            (match (cons (bag-case->type t-case)
                         (bag-case->type u-case))
              [(cons t (TVectorof inner-u const-expr-u))
               (if (equal? inner-u t)
                 (TVectorof t (normal-form `(+ 1 ,const-expr-u)))
                 (context-error "Cannot cons type ~a to a vector of ~a"
                                (type->string t)
                                (type->string inner-u)))]
              [(cons t (TVector u-list))
               (TVector (cons t u-list))]
              ; TODO I don't think we have a byte type?
              ;[(cons (TBytes n)
              ;       (TBytes m)) (TBytes (normal-form `(+ ,n ,m)))]
              [_ (context-error "cannot cons type ~a to a ~a"
                                (type->string t)
                                (type->string u))]))))

(: type-append (-> Type Type Type))
(define (type-append t u)
  ;; convert to bags
  (define t-bag (type->bag t))
  (define u-bag (type->bag u))
  ;; cartesian-product the two bags
  (for*/fold ([accum : Type (TNone)])
             ([t-case (Type-Bag-inner t-bag)]
              [u-case (Type-Bag-inner u-bag)]) : Type
    (TUnion accum
            (match (cons (bag-case->type t-case)
                         (bag-case->type u-case))
              [(cons (TVectorof inner-t const-expr-t)
                     (TVectorof inner-u const-expr-u))
               (if (equal? inner-u inner-t)
                 (TVectorof inner-t
                            (normal-form `(+ ,const-expr-t ,const-expr-u)))
                 (context-error "Inner types of vectors must match in append,
                                ~a and ~a"
                                (type->string inner-t)
                                (type->string inner-u)))]
              [(cons (TVectorof inner-t const-expr-t) (TVector u-list))
                 (if (subtype-of? (tvector-inner-type u-list) inner-t)
                   (TVectorof inner-t
                              (normal-form `(+ ,const-expr-t ,(length u-list))))
                   (context-error "cannot append a vector with differing types ~a to a ~a"
                                  (type->string t)
                                  (type->string u)))]
              [(cons (TVector u-list) (TVectorof inner-t const-expr-t))
                 (if (subtype-of? (tvector-inner-type u-list) inner-t)
                   (TVectorof inner-t
                              (normal-form `(+ ,const-expr-t ,(length u-list))))
                   (context-error "cannot append a vector with differing types ~a to a ~a"
                                  (type->string t)
                                  (type->string u)))]
              [(cons (TVector t-list)
                     (TVector u-list)) (TVector (append t-list u-list))]
              [(cons (TBytes n)
                     (TBytes m)) (TBytes (normal-form `(+ ,n ,m)))]
              [_ (context-error "cannot append types ~a and ~a"
                                (type->string t)
                                (type->string u))]))))

;; Given a "template" containing type variables and another type without type variables, return
;; a mapping from type variable to type
(: type-unify (-> Type Type (Values (HashTable TVar Type)
                                    (HashTable Symbol Const-Expr))))
(define (type-unify template type)
  (define template-bag (type->bag template))
  (define type-bag (type->bag type))
  ; Surprisingly easy: we just go through the bag and ask "where" are the type variables.
  ; Then, we bag-project those locations in the type and convert back to a type.
  (: tvar-locations (Setof (Listof (List TVar Prim-Index))))
  (define tvar-locations
    (for/set ([bag-case (Type-Bag-inner template-bag)]) : (Setof (Listof (List TVar Prim-Index)))
      (for/list ([(key value) bag-case]
                 #:when (match value
                          [(PVar a) #t]
                          [_ #f])) : (Listof (List TVar Prim-Index)) 
        (list (match value
                [(PVar a) (TVar a)]) key))))
  (: cg-locations (Setof (Listof (List Const-Expr Prim-Index))))
  (define cg-locations
    (for/set ([bag-case (Type-Bag-inner template-bag)]) : (Setof (Listof (List Const-Expr Prim-Index)))
      (for/list ([(key value) bag-case]
                 #:when (match value
                          [(and (? const-expr? a)
                                (not (? integer? a))) #t]
                          [_ #f])) : (Listof (List Const-Expr Prim-Index))
        (list (match value
                [(? const-expr? a) a]) key))))
  ; now we use those locations to lookup the type
  (: tvars (Listof TVar))
  (define tvars (remove-duplicates
                 (append*
                  (for/list ([case tvar-locations]) : (Listof (Listof TVar))
                    (for/list ([inner case]) : (Listof TVar)
                      (cast (first inner) TVar))))))
  (: cgvars (Listof Symbol))
  (define cgvars (remove-duplicates
                  (append*
                   (for/list ([case cg-locations]) : (Listof (Listof Symbol))
                     (append*
                      (for/list ([inner case]) : (Listof (Listof Symbol))
                        (const-expr->symbols (cast (first inner) Const-Expr))))))))
  (define dummy-filled
    (cg-template-fill
                        (type-template-fill template (for/hash ([tvar tvars]) : (HashTable TVar Type)
                                                       (values tvar (TAny))))
                        (hash)))
  (unless (subtype-of? type
                       dummy-filled)
    (context-error "cannot unify because ~a is not a subtype of ~a with all variables replaced by Any"
                   (type->string type)
                   (type->string template)))
  ;; the type table 
  (define type-table
    (for/hash ([tvar tvars]) : (HashTable TVar Type)
      (values tvar
              (bag->type
               (type->bag
                (for/fold ([accum : Type (TNone)])
                          ([location-case tvar-locations])
                  (define my-locations (for/list ([k location-case]
                                                  #:when (equal? (first k) tvar)) : (Listof Prim-Index)
                                         (cast (second k) Prim-Index)))
                  (TUnion accum
                          (for/fold ([accum : Type (TNone)])
                                    ([location my-locations]) : Type
                            (TUnion accum
                                    (bag->type (bag-project type-bag location)))))))))))
  ;; the cg table
  (define cg-table
    (for/fold ([accum : (HashTable Symbol Const-Expr) (hash)])
              ([location-case cg-locations])
      (for/fold ([accum accum])
                 ([(k v)
                  (for/hash ([expression-and-location location-case]) :
                    (HashTable Symbol Const-Expr)
                    (match expression-and-location
                      [(list expr location)
                       (cond
                         [(symbol? expr) (values expr
                                                 (let ([subbag (bag-project type-bag (cast location Prim-Index))])
                                                   (match (set->list (Type-Bag-inner subbag))
                                                     [(list one-elem) (let ([lala (hash-ref one-elem 'root #f)])
                                                                       (cond
                                                                         [(const-expr? lala) (normal-form lala)]
                                                                         [else (context-error "cannot reduce const-generic parameter ~a to an integer" expr)]))]
                                                     [_ (context-error "ambiguous const-generic parameter ~a" expr)])))]
                         [else (context-error "no support for unifying non-trivial const-generic parameter ~a at the moment"
                                              expr)])]))])
        (hash-set accum k v))))
  (values type-table cg-table))
  

;; Applies a type-variable mapping to a template, filling in the slots
(: type-template-fill (-> Type (HashTable TVar Type) Type))
(define (type-template-fill type table)
  (define recurse (λ((t : Type)) (type-template-fill t table)))
  (match type
    [(TVar _) (hash-ref table type)]
    [(TVectorof t n) (TVectorof (recurse t) n)]
    [(TVector lst) (TVector (map recurse lst))]
    [(TUnion x y) (TUnion (recurse x)
                          (recurse y))]
    [(TIntersect x y) (TIntersect (recurse x)
                                  (recurse y))]
    [x x]))

;; Picks out the const-generic symbols from a const-expr
(: const-expr->symbols (-> Const-Expr (Listof Symbol)))
(define (const-expr->symbols cexpr)
  (match cexpr
    [(? symbol?) (list cexpr)]
    [(list op x y) (append (const-expr->symbols x)
                           (const-expr->symbols y))]
    [x empty]))

;; Applies a const-generic mapping to a template, filling in the slots
(: cg-template-fill (-> Type (HashTable Symbol Const-Expr) Type))
(define (cg-template-fill type table)
  (define recurse (λ((t : Type)) (cg-template-fill t table)))
  ;; Helper: find-and-replace within a const-generic expression
  (: helper (-> Const-Expr Const-Expr))
  (define (helper cexpr)
    (normal-form (match cexpr
      [(? symbol?) (hash-ref table cexpr)]
      [(list op x y) (list op (helper x)
                           (helper y))]
      [x x])))
  (match type
    [(TVectorof t n) (let ([n-replaced (with-handlers ([exn:fail? (λ _ #f)]) (helper n))])
                       (if n-replaced (TVectorof (recurse t) n-replaced)
                           (TDynVectorof (recurse t))))]
    [(TVector lst) (TVector (map recurse lst))]
    [(TUnion x y) (TUnion (recurse x)
                          (recurse y))]
    [(TIntersect x y) (TIntersect (recurse x)
                                  (recurse y))]
    [x x]))


(type-unify (TVectorof (TVar 'a) 'N)
            (TVectorof (TNat) 3))
