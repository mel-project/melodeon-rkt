#lang typed/racket
(require "types.rkt"
         "../common.rkt"
         racket/hash)

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
                                  (context-error "attempted to add incompatible ~a = ~a to ~a"
                                                 key
                                                 value
                                                 case))]
    [else (hash-set case key value)]))

(: case-union (-> Bag-Case Bag-Case Bag-Case))
(define (case-union c1 c2)
  (for/fold ([accum c1])
            ([(k v) c2])
    (case-set accum k v)))

(define-type Prim-Index (U 'root
                           (List 'len Prim-Index)
                           (List 'ref Prim-Index Integer)))

(struct PNat () #:transparent)
(struct PVec () #:transparent)
(struct PBytes () #:transparent)
(define-type Prim-Type (U PNat PVec Index PBytes))

;; Converts a type belonging to the given index to a type-bag
(: type->bag (-> Type Prim-Index Type-Bag))
(define (type->bag type idx)
  (match type
    ; primitives
    [(TNat) (Type-Bag (set (hash idx (PNat))))]
    ; vectors: pairwise
    [(TBytes n) (Type-Bag (set (cast
                                (hash idx (PBytes)
                                      `(len ,idx) (ann n : Integer))
                                Bag-Case)))]
    [(TVectorof t n) (type->bag (TVector (make-list n t)) idx)]
    [(TVector types)
     (for/fold ([accum (Type-Bag (set (hash idx (PVec)
                                            `(len ,idx) (length types))))])
               ([type types]
                [ctr (in-naturals)]) : Type-Bag
       (pretty-print accum)
       (pretty-print (type->bag type `(ref ,idx ,ctr)))
       (bag-product accum (type->bag type `(ref ,idx ,ctr))))]
    ; union: just union them all
    [(TUnion t u) (bag-union (type->bag t idx)
                             (type->bag u idx))]
    ; intersect: cartesian product
    [(TIntersect t u) (bag-product (type->bag t idx)
                                   (type->bag u idx))]))

;; Bag-based subtype function
(: bag-subtype-of? (-> Type-Bag Type-Bag Boolean))
(define (bag-subtype-of? t u)
  ; trivial case: it's just a subset
  ; does this always work?
  (subset? (Type-Bag-inner t)
           (Type-Bag-inner u)))

#;(type->bag (TVector (list (TBytes 1))) 'root)

(bag-subtype-of? (type->bag (TVectorof (TUnion (TNat) (TBytes 1)) 1) 'root)
                 (type->bag
                  (TUnion (TVectorof (TNat) 1)
                          (TVectorof (TBytes 1) 1)) 'root))
