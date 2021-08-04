#lang typed/racket
(require "types.rkt"
         "../common.rkt"
         racket/hash)
(provide type->bag
         Type-Bag
         bag-subtract)

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
  (Type-Bag
   (for/set ([inner (Type-Bag-inner bag)]
             #:when (not (hash-empty? inner))) : (Setof Bag-Case)
     inner)))

(: type->bag/raw (-> Type Prim-Index Type-Bag))
(define (type->bag/raw type idx)
  (match type
    ; primitives
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
  (subset? (Type-Bag-inner t)
           (Type-Bag-inner u)))

;; Subtracts two bags. Removes all cases from b1 that are subsets of all cases of b2.
(: bag-subtract (-> Type-Bag Type-Bag Type-Bag))
(define (bag-subtract b1 b2)
  (bag-cleanup
   (Type-Bag
    (for/set ([case (Type-Bag-inner b1)]) : (Setof Bag-Case)
      (for/fold ([accum case])
                ([their-case (Type-Bag-inner b2)]) : Bag-Case
        (case-subtract accum their-case))))))

(: case-subtract (-> Bag-Case Bag-Case Bag-Case))
(define (case-subtract c1 c2)
  (for/hash ([(k v) c1]
             #:when (not (equal? (hash-ref c2 k #f) v))) : Bag-Case
    (values k v)))