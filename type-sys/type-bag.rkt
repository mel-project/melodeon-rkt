#lang typed/racket
(require "types.rkt"
         "../common.rkt"
         racket/hash)
(provide type->bag
         Type-Bag
         bag-subtract
         )

(define-type Bag-Case (HashTable Prim-Index Prim-Type))

(struct Type-Bag ((inner : (Setof Bag-Case)))
  #:transparent)

(: bag-union (-> Type-Bag Type-Bag Type-Bag))
(define (bag-union t u)
  (Type-Bag (set-union (Type-Bag-inner t)
                       (Type-Bag-inner u))))

(: bag-product (-> Type-Bag Type-Bag Type-Bag))
(define (bag-product t u)
  (Type-Bag
   (for*/set ([t-case (Type-Bag-inner t)]
              [u-case (Type-Bag-inner u)]) :
     (Setof Bag-Case)
     (case-union t-case
                 u-case))))

(: case-set (-> Bag-Case Prim-Index Prim-Type Bag-Case))
(define (case-set case key value)
  (cond
    [(hash-has-key? case key) (if (equal? (hash-ref case key) value)
                                  case
                                  (context-error "killing the whole thing"))]
    [else (hash-set case key value)]))

(: case-union (-> Bag-Case Bag-Case Bag-Case))
(define (case-union c1 c2)
  (with-handlers ([exn:fail? (λ(_) (ann (hash) Bag-Case))])
    (for/fold ([accum c1])
              ([(k v) c2])
      (case-set accum k v))))

(define-type Prim-Index (U 'root
                           (List 'len Prim-Index)
                           (List 'ref Prim-Index Integer)))

(struct PNat () #:transparent)
(struct PVec () #:transparent)
(struct PBytes () #:transparent)
(define-type Prim-Type (U PNat PVec Index PBytes))

;; Converts a type belonging to the given index to a type-bag
(: type->bag (-> Type Type-Bag))
(define (type->bag type)
  (bag-cleanup (type->bag/raw type 'root)))

;; Cleans up a bag, by removing all empty elements
(: bag-cleanup (-> Type-Bag Type-Bag))
(define (bag-cleanup bag)
  bag)

(: type->bag/raw (-> Type Prim-Index Type-Bag))
(define (type->bag/raw type idx)
  (match type
    ; primitives
    [(TAny) (Type-Bag (set (ann (hash) Bag-Case)))]
    [(TNat) (Type-Bag (set (hash idx (PNat))))]
    ; vectors: pairwise
    [(TBytes n) (Type-Bag (set (cast
                                (hash idx (PBytes)
                                      `(len ,idx) (ann n : Integer))
                                Bag-Case)))]
    [(TVectorof t n) (type->bag/raw (TVector (make-list n t)) idx)]
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
(define (bag-subtype-of? t u)
  ; trivial case: it's just a subset
  ; does this always work?
  ; no it doesn't, haha
  (subset? (Type-Bag-inner t)
           (Type-Bag-inner u)))

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
            (if subtracted (hash-set accum
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