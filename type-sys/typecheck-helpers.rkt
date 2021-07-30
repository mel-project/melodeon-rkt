#lang typed/racket
(require "types.rkt"
         "../ast-utils.rkt"
         "resolver.rkt"
         "../common.rkt"
         racket/hash)
(provide (all-defined-out))

;; Given a type and a type->type function, creates another type, made through invoking the given function only with non-logical types. This is used for generic type inference, vector indexing, etc.
(: type-inner-map (-> Type (-> Type Type) Type))
(define (type-inner-map type type-deconstruct)
  (: clause-map (-> (Setof Type) Type))
  (define (clause-map clause)
    (foldl (λ((type : Type)
              (accum : Type))
             (TIntersect (type-deconstruct type) accum))
           (TAny)
           (set->list clause)))
  ;; break down into dnf
  (define dnf (type->dnf type))
  (displayln (dnf->string dnf))
  (foldl (λ((clause : (Setof Type))
            (t : Type))
           (TUnion (clause-map clause)
                   t))
         (TNone)
         (set->list dnf)))

;; Given a type and an index, gets the output type
(: tindex (-> Type Nonnegative-Integer Type))
(define (tindex vector-type idx)
  (: inner (-> Type Type))
  (define (inner vector-type)
    (match vector-type
      [(TVector lst) (if (>= idx (length lst))
                         (gen-tfail vector-type)
                         (list-ref lst idx))]
      [(TVectorof t n) (if (>= idx n)
                           (gen-tfail vector-type)
                           t)]
      [(TBytes n) (if (>= idx n)
                      (gen-tfail vector-type)
                      (TNat))]
      ;; set theory types
      [(TNegate t) (TNegate (inner t))]
      [(TUnion t u) (TUnion (inner t)
                            (inner u))]
      [(TIntersect t u) (TIntersect (inner t)
                                    (inner u))]
      ;; unindexable types
      [(TNone) (TNone)]
      [_ (gen-tfail vector-type)]))
  ;; check that it's not a fail type
  (let ([res (inner vector-type)])
    (if (subtype-of? res (TAny))
        res
        (context-error "cannot index into position ~a of ~a"
                       idx
                       (type->string vector-type)))))

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

(: tf-negate (-> Type-Facts Type-Facts))
(define (tf-negate tf1)
  (for/hash ([(k v) tf1]) : (Immutable-HashTable Symbol Type)
    (values k (TNegate v))))

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
    [`(@type-var Bin) (TBin)]
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
    [_ (error "wtf man" texpr)]
    ))

;; "smart union" of two types that doesn't create a TUnion if one is the subtype of another
(: smart-union (-> Type Type Type))
(define (smart-union t1 t2)
  (cond
    [(subtype-of? t1 t2) t2]
    [(subtype-of? t2 t1) t1]
    [else (TUnion t1 t2)]))