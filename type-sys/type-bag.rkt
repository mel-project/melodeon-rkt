#lang typed/racket
(require "types.rkt"
         "../common.rkt"
         racket/hash)
(provide type->bag
         (struct-out Type-Bag)
         bag-subtract
         bag-project
         bag-case->type
         Prim-Index
         PVar
         bag->type
         bag-subtype-of?
         )


(struct PNat () #:transparent)
(struct PVec () #:transparent)
(struct PTagged ((tag : Symbol)) #:transparent)
(struct PVar ((sym : Symbol)) #:transparent)
(struct PBytes () #:transparent)
(define-type Prim-Type (U PNat PVec PVar Integer PBytes PTagged))

(define-type Prim-Index (U 'root
                           (List 'len Prim-Index)
                           (List 'ref Prim-Index Integer)
                           (List 'all-ref Prim-Index)))


(define-type Bag-Case (HashTable Prim-Index Prim-Type))

(struct Type-Bag ((inner : (Setof Bag-Case)))
  #:transparent)

(: bag-project (-> Type-Bag Prim-Index Type-Bag))
(define (bag-project bag pidx)
  (Type-Bag
   (for/set ([bag-case (Type-Bag-inner bag)]) : (Setof Bag-Case)
     (for/fold ([accum (ann (hash) (HashTable Prim-Index Prim-Type))])
               ([(key val) bag-case]) : (HashTable Prim-Index Prim-Type)
       (with-handlers ([exn:fail? (λ _ accum)])
         (hash-set accum (pidx-project key pidx) val))))))

(: bag-union (-> Type-Bag Type-Bag Type-Bag))
(define (bag-union t u)
  (Type-Bag (set-union (Type-Bag-inner t)
                       (Type-Bag-inner u))))

(: bag-product (-> Type-Bag Type-Bag Type-Bag))
(define (bag-product t u)
  (Type-Bag
   (for/set ([elem
              (for*/set ([t-case (Type-Bag-inner t)]
                         [u-case (Type-Bag-inner u)]) :
                (Setof (Option Bag-Case))
                (with-handlers ([exn:fail? (λ _ #f)])
                  (case-union t-case
                              u-case)))]
             #:when elem) : (Setof Bag-Case)
     elem)))

(: case-ref (-> Bag-Case Prim-Index (Option Prim-Type)))
(define (case-ref case key)
  (cond
    [(hash-has-key? case key) (hash-ref case key)]
    [else (match key
            [`(ref ,inner ,_) (hash-ref case `(all-ref ,inner) #f)]
            [`(all-ref ,inner)
             (with-handlers ([exn:fail? (λ _ #f)])
               (define types
                 (remove-duplicates
                  (for/list ([idx (cast (hash-ref case `(len ,inner)) Integer)]) : (Listof Prim-Type)
                    (hash-ref case `(ref ,inner ,idx)))))
               (if (= 1 (length types)) (car types) #f))]
            [_ #f])]))

(: case-set (-> Bag-Case Prim-Index Prim-Type Bag-Case)) 
(define (case-set case key value)
  (cond
    [(case-ref case key) (if (equal? (case-ref case key) value)
                             case
                             (context-error "killing the whole thing"))]
    [else (hash-set case key value)]))

(: case-union (-> Bag-Case Bag-Case Bag-Case))
(define (case-union c1 c2)
  (for/fold ([accum c1])
            ([(k v) c2])
    (case-set accum k v)))


(: pidx-project (-> Prim-Index Prim-Index Prim-Index))
(define (pidx-project pidx new-root)
  (cond
    [(equal? pidx new-root) 'root]
    [else (match pidx
            [`(ref ,inner ,len) `(ref ,(pidx-project inner new-root) ,len)]
            [`(len ,inner) `(len ,(pidx-project inner new-root))]
            [_ (error "cannot project")])]))
;; Converts a type belonging to the given index to a type-bag
(: type->bag (-> Type Type-Bag))
(define (type->bag type)
  (type->bag/raw type 'root))

(: type->bag/raw (-> Type Prim-Index Type-Bag))
(define (type->bag/raw type idx)
  (match type
    ; primitives
    [(TNone) (Type-Bag (set))]
    [(TAny) (Type-Bag (set (ann (hash) Bag-Case)))]
    [(TNat) (Type-Bag (set (hash idx (PNat))))]
    [(TVar a) (Type-Bag (set (hash idx (PVar a))))]
    ; vectors: pairwise
    [(TBytes n) (Type-Bag (set (cast
                                (hash idx (PBytes)
                                      `(len ,idx) (ann n : Integer))
                                Bag-Case)))]
    ; structs
    [(TTagged tag types)
     (for/fold ([accum (Type-Bag (set (hash idx (PTagged tag)
                                            `(len ,idx) (+ (length types) 1))))])
               ([type types]
                [ctr (in-naturals)]) : Type-Bag
       (bag-product accum (type->bag/raw type `(ref ,idx ,(+ 1 ctr)))))]
    [(TDynVectorof t) (bag-product
                       (Type-Bag (set (hash idx (PVec))))
                       (type->bag/raw t `(all-ref ,idx)))]
    [(TDynBytes) (Type-Bag (set (hash idx (PBytes))))]
    [(TVectorof t n) (bag-product
                      (Type-Bag (set (hash idx (PVec)
                                           `(len ,idx) n)))
                      (type->bag/raw t `(all-ref ,idx)))]
    [(TVector types)
     (for/fold ([accum (Type-Bag (set (hash idx (PVec)
                                            `(len ,idx) (length types))))])
               ([type types]
                [ctr (in-naturals)]) : Type-Bag
       ;(pretty-print accum)
       ;(pretty-print (type->bag/raw type `(ref ,idx ,ctr)))
       (bag-product accum (type->bag/raw type `(ref ,idx ,ctr))))]
    ; union: just union them all
    [(TUnion t u) (bag-union (type->bag/raw t idx)
                             (type->bag/raw u idx))]
    ; intersect: cartesian product
    [(TIntersect t u) (bag-product (type->bag/raw t idx)
                                   (type->bag/raw u idx))]))

;; Bag-based subtype function
(: bag-subtype-of? (-> Type-Bag Type-Bag Boolean))
(define (bag-subtype-of? u t)
  ; every case in t must be a superset of all the cases in u
  (for/and ([t-case (Type-Bag-inner t)]) : Boolean
    (for/or ([u-case (Type-Bag-inner u)]) : Boolean
      (for/and ([(t-key t-val) t-case]) : Boolean
        (and (case-ref u-case t-key)
             (equal? t-val (case-ref u-case t-key)))))))

(: set-union* (All (a) (-> (Setof (Setof a)) (Setof a))))
(define (set-union* sets)
  (for/fold ([accum (ann (set) (Setof a))])
            ([one-set sets]) : (Setof a)
    (set-union accum one-set)))

;; Subtracts two bags. Removes all cases from b1 that are subsets of all cases of b2.
(: bag-subtract (-> Type-Bag Type-Bag Type-Bag))
(define (bag-subtract b1 b2)
  ; subtract all cases in b2 from every case in b1
  (Type-Bag
   (for/fold ([outer-accum (ann (set) (Setof Bag-Case))])
             ([b1-case (Type-Bag-inner b1)]) : (Setof Bag-Case)
     (set-union outer-accum 
                (set-union*
                 (for/set ([b2-case (Type-Bag-inner b2)]) : (Setof (Setof Bag-Case))
                   (case-subtract b1-case b2-case)))))))



(: case-subtract (-> Bag-Case Bag-Case (Setof Bag-Case)))
(define (case-subtract c d)
  ; for every term in d, subtract that from every term in c
  (cast 
   (set-remove 
    (for/set ([(d-term-k d-term-v) d]) : (Setof (Option Bag-Case))
      (with-handlers ([exn:fail? (λ _ #f)])
        (for/fold ([accum (ann (hash) Bag-Case)])
                  ([(c-term-k c-term-v) c]) : Bag-Case
          (let ([subtracted (term-subtract (cons c-term-k c-term-v)
                                           (cons d-term-k d-term-v))])
            (if subtracted (case-set accum
                                     (car subtracted)
                                     (cdr subtracted))
                (error "fail now"))))))
    #f)
   (Setof Bag-Case)))

(: term-subtract (-> (Pair Prim-Index Prim-Type)
                     (Pair Prim-Index Prim-Type)
                     (Option (Pair Prim-Index Prim-Type))))
(define (term-subtract term-1 term-2)
  (cond
    ; if the terms are identical, fail. that's pretty obvious
    [(equal? term-1 term-2) #f]
    ; otherwise, we conservatively no-op it
    [else term-1]))

(: bag-case->type (-> Bag-Case Type))
(define (bag-case->type case)
  (bag-case->type/inner case 'root))

(: bag->type (-> Type-Bag Type))
(define (bag->type bag)
  (for/fold ([accum : Type (TNone)])
            ([bag-case (Type-Bag-inner bag)]) : Type
    (if (equal? accum (TNone)) (bag-case->type bag-case)
        (TUnion (bag-case->type bag-case) accum))))


(: bag-case->type/inner (-> Bag-Case Prim-Index Type))
(define (bag-case->type/inner case pidx)
  (match (hash-ref case pidx #f)
    [#f (TAny)]
    [(PVar t) (TVar t)]
    [(PNat) (TNat)]
    [(PVec) (or (with-handlers ([exn:fail? (λ _ #f)])
                  (define length (cast (hash-ref case `(len ,pidx)) Integer))
                  (TVector (for/list ([i length]) : (Listof Type)
                             (bag-case->type/inner case `(ref ,pidx ,i)))))
                (with-handlers ([exn:fail? (λ _ (TAny))])
                  (define inner-type (bag-case->type/inner (hash 'root
                                                                 (hash-ref case `(all-ref ,pidx)))'root))
                  (TDynVectorof inner-type)))]
    [(PBytes) (or (with-handlers ([exn:fail? (λ _ #f)])
                    (define length (cast (hash-ref case `(len ,pidx)) Nonnegative-Integer))
                    (TBytes length))
                  (with-handlers ([exn:fail? (λ _ (TAny))])
                    (TDynBytes)))]))