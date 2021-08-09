#lang typed/racket
(require "types.rkt"
         "../ast-utils.rkt"
         "typecheck-unify.rkt"
         "../common.rkt"
         "type-bag.rkt"
         racket/hash)
(provide (all-defined-out))


; Produce the name of a product type's
; accessor function for a given field
(: accessor-name (-> Symbol Symbol Symbol))
(define (accessor-name type field)
  (string->symbol (format "~a-~a" type field)))

; The inner type is just the union of all
; types in the type vector
(: tvector-inner-type (-> TVector Type))
(define (tvector-inner-type tvec)
  (define v (TVector-lst tvec))
  (foldl (λ((t : Type) (acc : Type)) (TUnion t acc))
         (car v)
         (cdr v)))

;; A function
(struct TFunction ((arg-types : (Listof Type))
                   (result-type : Type)))

;; A type scope
(struct Type-Scope ((vars : Type-Map)
                    (type-vars : Type-Map)
                    (bound-facts : (Immutable-HashTable Symbol Type-Facts))
                    (funs : (Immutable-HashTable Symbol TFunction))) #:prefab)

(: ts-empty Type-Scope)
(define ts-empty (Type-Scope (hash) (hash) (hash) (hash)))

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
    (Type-Scope-bound-facts x)
    (Type-Scope-bound-facts y))
   (hash-union
    (make-immutable-hash (hash->list (Type-Scope-funs x)))
    (make-immutable-hash (hash->list (Type-Scope-funs y))))))

(define-type Type-Facts (Immutable-HashTable Symbol Type))

(: tf-empty Type-Facts)
(define tf-empty (hash))

(: tf-union (-> Type-Facts Type-Facts Type-Facts))
(define (tf-union tf1 tf2)
  (foldl (λ((a : (Pair Symbol Type))
            (accum : Type-Facts))
           (add-fact accum (car a) (cdr a)))
         tf2
         (hash->list tf1)))

(: apply-facts (-> Type-Scope Type-Facts Type-Scope))
(define (apply-facts ts tf)
  (foldl
   (lambda ((kv : (Pair Symbol Type))
            (rst : Type-Scope))
     (define corresponding (hash-ref (Type-Scope-vars ts) (car kv) #f))
     (if corresponding
         (bind-var rst
                   (car kv)
                   (TIntersect corresponding (cdr kv)))
         rst))
   ts
   (hash->list tf)))

(: subtract-facts (-> Type-Scope Type-Facts Type-Scope))
(define (subtract-facts ts tf)
  (foldl
   (lambda ((kv : (Pair Symbol Type))
            (rst : Type-Scope))
     (define corresponding (hash-ref (Type-Scope-vars ts) (car kv) #f))
     (if corresponding
         (bind-var rst
                   (car kv)
                   (bag->type (bag-subtract (type->bag corresponding) (type->bag (cdr kv)))))
         rst))
   ts
   (hash->list tf)))

(: add-fact (-> Type-Facts Symbol Type Type-Facts))
(define (add-fact tf sym type)
  (hash-set tf sym
            (cond
              [(hash-has-key? tf sym) (TIntersect type (hash-ref tf sym))]
              [else type])))

(: bind-facts (-> Type-Scope Symbol Type-Facts Type-Scope))
(define (bind-facts ts var-name var-facts)
  (match ts
    [(Type-Scope vars type-vars type-facts funs)
     (Type-Scope vars type-vars (hash-set type-facts var-name var-facts) funs)]))


(: bind-var (-> Type-Scope Symbol Type Type-Scope))
(define (bind-var ts var-name var-type)
  (match ts
    [(Type-Scope vars type-vars type-facts funs)
     (Type-Scope (hash-set vars var-name var-type) type-vars type-facts funs)]))

(: bind-fun (-> Type-Scope Symbol TFunction Type-Scope))
(define (bind-fun ts fun-name fun-type)
  (match ts
    [(Type-Scope vars type-vars type-facts funs)
     (Type-Scope vars type-vars type-facts (hash-set funs fun-name  fun-type))]))

(: bind-type-var (-> Type-Scope Symbol Type Type-Scope))
(define (bind-type-var ts var-name var-type)
  (match ts
    [(Type-Scope vars type-vars type-facts funs)
     (Type-Scope vars (hash-set type-vars var-name var-type) type-facts funs)]))

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

;; Resolves a type or throws an error
(: resolve-type (-> Type-Expr Type-Map Type))
(define (resolve-type texpr env)
  (match texpr
    [`(@type-var Any) (TAny)]
    [`(@type-var Nat) (TNat)]
    ;[`(@type-var ,var) (lookup-type-var 
    ;[`(@type-var ,var) (context-error "cannot resolve type names yet")]
    ;[`(@type-vec ,vec) (TVector (map (lambda (x) (resolve-type x env)) vec))]
    [`(@type-vec ,vec) (TVector (map (λ((x : Type-Expr)) (resolve-type x env)) vec))]
    [`(@type-vecof ,var ,count) (TVectorof (resolve-type var env) count)]
    [`(@type-bytes ,count) (TBytes count)]
    [`(@type-union ,x ,y)
     (TUnion (resolve-type x env)
             (resolve-type y env))]
    [`(@type-struct ,name ,fields)
      (TTagged
        name
        (map (lambda ([x : (List Symbol Type-Expr)])
               (resolve-type (cadr x) env))
             fields))]
    [_ (error "wtf man" texpr)]
    ))

;; "smart union" of two types that doesn't create a TUnion if one is the subtype of another
(: smart-union (-> Type Type Type))
(define (smart-union t1 t2)
  (cond
    [(subtype-of? t1 t2) t2]
    [(subtype-of? t2 t1) t1]
    [else (TUnion t1 t2)]))