#lang typed/racket
(require "type-bag.rkt"
         "types.rkt"
         "../asts/raw-ast.rkt")
(provide type-index
         type-append
         type-unify
         type-template-fill
         subtype-of?)

(: subtype-of? (-> Type Type Boolean))
(define (subtype-of? t1 t2)
  (bag-subtype-of? (type->bag t1)
                   (type->bag t2)))

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

(: type-append (-> Type Type Type))
(define (type-append t u)
  ;; convert to bags
  (define t-bag (type->bag t))
  (define u-bag (type->bag u))
  ;; cartesian-product the two bags
  (for*/fold ([accum : Type (TNone)])
             ([t-case (Type-Bag-inner t-bag)]
              [u-case (Type-Bag-inner u-bag)]) : Type
    ;; This *should* work.
    (TUnion accum
            (match (cons (bag-case->type t-case)
                         (bag-case->type u-case))
              [(cons (TVector t-list)
                     (TVector u-list)) (TVector (append t-list u-list))]
              [(cons (TBytes n)
                     (TBytes m)) (TBytes `(+ ,n ,m))]
              [_ (context-error "cannot append types ~a and ~a"
                                (type->string t)
                                (type->string u))]))))

;; Given a "template" containing type variables and another type without type variables, return
;; a mapping from type variable to type
(: type-unify (-> Type Type (HashTable TVar Type)))
(define (type-unify template type)
  (define template-bag (type->bag template))
  (define type-bag (type->bag type))
  ; Surprisingly easy: we just go through the bag and ask "where" are the type variables.
  ; Then, we bag-project those locations in the type and convert back to a type.
  (: tvar-locations  (Setof (Listof (List TVar Prim-Index))))
  (define tvar-locations
    (for/set ([bag-case (Type-Bag-inner template-bag)]) : (Setof (Listof (List TVar Prim-Index)))
      (for/list ([(key value) bag-case]
                 #:when (match value
                          [(PVar a) #t]
                          [_ #f])) : (Listof (List TVar Prim-Index)) 
        (list (match value
                [(PVar a) (TVar a)]) key))))
  ;(pretty-print tvar-locations)
  ; now we use those locations to lookup the type
  (: tvars (Listof TVar))
  (define tvars (remove-duplicates
                 (append*
                  (for/list ([case tvar-locations]) : (Listof (Listof TVar))
                    (for/list ([inner case]) : (Listof TVar)
                      (cast (first inner) TVar))))))
  (unless (subtype-of? type
                       (type-template-fill template (for/hash ([tvar tvars]) : (HashTable TVar Type)
                                                      (values tvar (TAny)))))
    (context-error "cannot unify because ~a is not a subtype of ~a with all variables replaced by Any"
                   (type->string type)
                   (type->string template)))
  (for/hash ([tvar tvars]) : (HashTable TVar Type)
    (values tvar
            (bag->type
             (type->bag
              (for/fold ([accum : Type (TNone)])
                        ([location-case tvar-locations])
                (define my-locations (for/list ([k location-case]
                                                #:when (equal? (first k) tvar)) : (Listof Prim-Index)
                                       (cast (second k) Prim-Index)))
                ;(pretty-print my-locations)
                (TUnion accum
                        (for/fold ([accum : Type (TNone)])
                                  ([location my-locations]) : Type
                          (TUnion accum
                                  (bag->type (bag-project type-bag location)))))))))))

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

#;(type-unify (TUnion (TVar 'a) (TNat))
            (TUnion (TNat) (TBytes 5)))

; Substitute variables in a constant expression with numbers
; and simplify into a number
(: subst-const-expr (-> Const-Expr
                        (HashTable Symbol Nonnegative-Integer)
                        Nonnegative-Integer))