#lang typed/racket
(require "types.rkt"
         "../asts/raw-ast.rkt"
         racket/hash)

; For constant expressions
(define-type Sexpr (U (Listof Any) Any))
(require/typed rascas
               [substitute (-> Sexpr Sexpr Sexpr Sexpr)]
               [automatic-simplify (-> Sexpr Sexpr)]
               [algebraic-expand (-> Sexpr Sexpr)])

(provide type->bag
         (struct-out Type-Bag)
         empty-bag
         bag-subtract
         bag-project
         bag-product 
         bag-union
         bag-case->type
         Prim-Index
         PVar
         bag->type 
         bag-subtype-of?
         normal-form
         subst-const-expr
         )


(struct PNat () #:transparent)
(struct PNatRange ((start : (Option Const-Expr))
                   (end : (Option Const-Expr)))
  #:transparent)
(struct PVec () #:transparent)
(struct PTagged ((tag : Symbol)) #:transparent)
(struct PVar ((sym : Symbol)) #:transparent)
(struct PBytes () #:transparent)

(: normal-form (-> Const-Expr Const-Expr))
(define (normal-form e)
  (cast (algebraic-expand e) Const-Expr))

; Substitute a symbol in a constant expression with another
; const expression.
(: subst-const-expr (-> Const-Expr Symbol Const-Expr Const-Expr))
(define (subst-const-expr e var sub-e)
  (normal-form (cast (substitute e var sub-e) Const-Expr)))

;(algebraic-expand '(* (+ x 1) (- x 1)))
;(algebraic-expand '(+ N 1))
;(algebraic-expand '(= (+ 1 x) (* (+ x 1) (- x 1))))
;(automatic-simplify '(* (+ x 1) (- x 1)))
;(algebraic-expand (substitute '(* (+ x 1) (- x 1)) 'x 3))

(define-type Prim-Type (U PNat PNatRange PVec PVar Const-Expr PBytes PTagged PNatRange))

(define-type Prim-Index (U 'root
                           (List 'len Prim-Index)
                           (List 'ref Prim-Index Const-Expr)
                           (List 'all-ref Prim-Index)))


(define-type Bag-Case (HashTable Prim-Index Prim-Type))

(struct Type-Bag ((inner : (Setof Bag-Case)))
  #:transparent)

(: empty-bag Type-Bag)
(define empty-bag (Type-Bag (set)))

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

; Get the prim-type associated with a prim-index from a
; Bag-Case if it exists
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

; Add the prim-index/prim-type key/value pair to a bag-case
; if it does not exist. If it already exists and they're equal
; return the bag-case as-is. If they aren't equal, fail.
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
            [`(all-ref ,inner) (match new-root
                                 [`(ref root ,_) inner]
                                 [_ `(all-ref ,(pidx-project inner new-root))])]
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
    [(TNatRange a b) (Type-Bag (set (hash idx (PNatRange a b))))]
    [(TVar a) (Type-Bag (set (hash idx (PVar a))))]
    ;[(TConst e) (Type-Bag (set (hash idx e)))]
    ; vectors: pairwise
    [(TBytes n) (Type-Bag (set (cast
                                (hash idx (PBytes)
                                      `(len ,idx) n
                                      `(all-ref ,idx) (PNat))
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
    [(TVectorof t e) (bag-product
                      (Type-Bag (set (hash idx (PVec)
                                           `(len ,idx) e)))
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
        (let ([inner (case-ref u-case t-key)])
          (and inner
               (prim-subtype-of? inner t-val)))))))

;; Most primitive subtyping relation
(: prim-subtype-of? (-> Prim-Type Prim-Type Boolean))
(define (prim-subtype-of? u t)
  (printf "subtyping ~v to ~v\n" u t)
  (match (cons u t)
    [(cons (PNatRange #f #f) _) (prim-subtype-of? (PNat) t)]
    [(cons _ (PNatRange #f #f)) (prim-subtype-of? u (PNat))]
    [(cons (PNatRange _ _) (PNat)) #t]
    [(cons (PNatRange my-start my-end)
           (PNatRange their-start their-end))
     (and (re<= their-start my-start)
          (re<= my-end their-end))]
    [_ 
     (equal? u t)]))

;; Compares to cexprs
(: re<= (-> (Option Const-Expr)
            (Option Const-Expr)
            Boolean))
(define (re<= a b)
  (printf "comparing ~v to ~v\n" a b)
  (cond
    [(not b) #t]
    [(not a) (not b)]
    [(and (integer? a)
          (integer? b))
     (<= a b)]
    ;; conservatively fail. maybe one day we'll do some cool arithmetic to prove ordering on two cexprs, but not this day.
    [else #f]))

(: set-union* (All (a) (-> (Setof (Setof a)) (Setof a))))
(define (set-union* sets)
  (for/fold ([accum (ann (set) (Setof a))])
            ([one-set sets]) : (Setof a)
    (set-union accum one-set)))

#|
(: bag-slice (-> Type-Bag Integer Integer Type-Bag))
(define (bag-slice bag from to)
  (Type-Bag (list->set (map
    (λ((bag-case : Bag-Case)) : (Listof Bag-Case)
      (filter (λ(pidx) (match pidx
                         [`(ref ,_ ,expr) #t]
                         [_ #f]))
              (hash->list bag-case)))
    (set->list (Type-Bag-inner bag))))))
|#


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
    [(PVec)
     (or
      ; if len is an int, make a TVector
      (with-handlers ([exn:fail? (λ _ #f)])
        (define length (cast (hash-ref case `(len ,pidx)) Integer))
        (TVector (for/list ([i length]) : (Listof Type)
                   (bag-case->type/inner case `(ref ,pidx ,i)))))

      ; otherwise, if len is a const-expr, try to make a TVectorof
      (let ([inner-type (with-handlers ([exn:fail? (λ _ (TAny))])
                          (bag-case->type/inner
                            (hash 'root (hash-ref case `(all-ref ,pidx)))'root))])
        (define len
          (with-handlers ([exn:fail? (λ _ #f)])
                         (hash-ref case `(len ,pidx))))

        (if (const-expr? len)
          (TVectorof inner-type len)
          ; if no len found, make it dynamic
          (TDynVectorof inner-type))))]

    [(PBytes) (or (with-handlers ([exn:fail? (λ _ #f)])
                    (define length (cast (hash-ref case `(len ,pidx)) Nonnegative-Integer))
                    (TBytes length))
                  (with-handlers ([exn:fail? (λ _ (TAny))])
                    (TDynBytes)))]))

(define T (TVectorof (TVector (list (TBytes 32)
                                    (TNat)
                                    (TNat)
                                    (TAny))) 1))