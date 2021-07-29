#lang typed/racket
(require racket/trace)
(require "../common.rkt"
         "types.rkt")
(require/typed "sat.rkt"
               [sat-solve (-> Any Any)])

(provide subtype-of?)

;; a total ordering of types. break types that are subtypes of each other by lexicographic order
(: type-smaller (-> Type Type Boolean))
(define (type-smaller t1 t2)
  (cond
    [(and (subtype-of? t1 t2)
          (subtype-of? t2 t1))
     (string<? (format "~a" t1)
               (format "~a" t2))]
    [else
     (subtype-of? t1 t2)]))

;; is t1 a subtype of t2
(: subtype-of? (-> Type Type Boolean))
(define (subtype-of? t1 t2)
  ; if "not (x in t1 => x in t2)" is satisfiable, then we return *false*. 
  (not (sat-solve `(not (or (not ,(type->sat t1))
                            ,(type->sat t2))))))

;; Translates a type to a SAT expression.
;; Vectors are essentially handled as "x in [Nat, Nat] => x_1 in Nat and x_2 in Nat and x in [Any * 2]"
(: type->sat (->* (Type) (Symbol) Any))
(define (type->sat type (var-name 'x))
  (match type
    [(TBin) (string->symbol (format "~a-bin" var-name))]
    [(TNat) ; a Nat is a union of a Bin and a NonBinNat
     `(or ,(string->symbol (format "~a-bin" var-name))
          ,(string->symbol (format "~a-nbin" var-name)))]
    [(TAny) ; always true
     #t]
    [(TBytes n) (string->symbol (format "~a-bytes-~a" var-name n))]
    [(TVectorof t n) (type->sat (TVector (make-list n t)) var-name)]
    [(TVectorEtc elems)
     (define subelems
       (for/list ([elem elems]
                  [count (in-naturals)]) : (Listof Any)
         (define sym (string->symbol (format "~a-~a" var-name count)))
         (type->sat elem sym)))
     ((inst foldl Any) (λ((elem : Any)
                          (accum : Any))
                         `(and ,elem
                               ,accum))
                       #t
                       subelems)]
    [(TVector elems)
     (define subelems
       (for/list ([elem elems]
                  [count (in-naturals)]) : (Listof Any)
         (define sym (string->symbol (format "~a-~a" var-name count)))
         (type->sat elem sym)))
     ((inst foldl Any) (λ((elem : Any)
                          (accum : Any))
                         `(and ,elem
                               ,accum))
                       (string->symbol (format "~a-vec-~a" var-name (length elems)))
                       subelems)]
    [(TUnion x y) `(or ,(type->sat x var-name)
                       ,(type->sat y var-name))]
    [(TIntersect x y) `(and ,(type->sat x var-name)
                            ,(type->sat y var-name))]
    [(TNegate x) `(not ,(type->sat x var-name))]))

#|
(define (subtype-of? t1 t2)
  (pretty-write `(subtype-of ,t1 ,t2))
  (match (cons t1 t2)
    ; "hardcoded" subtypes
    [(cons _ (TAny)) #t]
    [(cons (TBin) (TNat)) #t]
    ;  [Subtype, Subtype ..] <: [Supertype, Supertype ..]
    [(cons (TVector x)
           (TVector y)) (andmap subtype-of? x y)]
    ; "set" types
    [(cons (TUnion x y) z) (and (subtype-of? x z)
                                (subtype-of? y z))]
    [(cons (TIntersect x y) z) (or (subtype-of? x z)
                                   (subtype-of? y z))]
    [(cons x (TIntersect y1 y2)) (and (subtype-of? x y1)
                                      (subtype-of? x y2))]
    [(cons x (TUnion y1 y2)) (or (subtype-of? x y1)
                                 (subtype-of? x y2))]
    [(cons x (TNegate y)) (not (subtype-of? x y))]
    ; trivially equal types
    [(cons x y) (equal? x y)]))|#

(module+ test
  ; Nat | [Nat, Nat | #[5]]
  (define T (TUnion (TNat) (TVector (list (TNat) (TUnion (TNat) (TBytes 5))))))
  ; [Nat, Nat]
  (define U (TVector (list (TNat) (TNat))))
  ; U <: T ? 
  (time (subtype-of? T U))
  )

#|

def f<T>(x: [T * 6]) -> T = x[0]

f([1, 2, 3, 4, 5, 6])

find T such that [Nat * 6] <: [T * 6]

|#