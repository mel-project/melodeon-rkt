#lang typed/racket
(require racket/trace)
(require "../common.rkt"
         "types.rkt")
(require/typed "sat.rkt"
               [sat-solve (-> Any Any)])

(provide subtype-of?
         Type-DNF
         type->dnf
         dnf->string)

#|
Todo: generic type resolving. A simple case is resolving the return type of the indexing operator.

Idea for indexing:
 1. Turn type into "surface-level" DNF form
 2. Recurse on each clause, making the union of the index-operator on every clause:
    - Intersect each element of the clause:
    - If the clause is a vector by itself, then we're "done".
    - Otherwise, we die a horrible death.
|#

;; Type DNF form
(define-type Type-DNF (Setof (Setof Type)))

(: remove-duplicate-negation (-> Type Type))
(define (remove-duplicate-negation type)
  (match type
    [(TNegate (TNegate t)) (remove-duplicate-negation t)]
    [t t]))

(: remove-autonegate (-> (Listof Type) (Listof Type)))
(define (remove-autonegate types)
  (define negated (list->set (filter TNegate? types)))
  (define to-delete
    (list->set (filter (λ((x : Type))
                         (set-member? negated (TNegate x))) types)))
  (filter
   (λ((t : Type))
     (match t
       [(TNegate x) (not (set-member? to-delete x))]
       [x (not (set-member? to-delete x))])) types))
         
(: type->dnf (-> Type Type-DNF))
(define (type->dnf type)
  (list->set
   (filter (λ((x : (Setof Type))) (not (set-empty? x)))
           (for/list ([clause (type->dnf/raw type)]) : (Listof (Setof Type))
             (list->set
              (remove-autonegate
               (for/list ([elem clause]) : (Listof Type)
                 (remove-duplicate-negation elem))))))))

(: type->dnf/raw (-> Type Type-DNF))
(define (type->dnf/raw type)
  (match type
    [(TUnion x y) (set-union (type->dnf x)
                             (type->dnf y))]
    [(TIntersect x y) (list->set
                       (for*/list ([x-clause (type->dnf x)]
                                   [y-clause (type->dnf y)]) : (Listof (Setof Type))
                         (set-union x-clause y-clause)))]
    [(TNegate x) (list->set
                  (for/list ([clause (type->dnf x)]) : (Listof (Setof Type))
                    (list->set
                     (for/list ([elem clause]) : (Listof Type)
                       (TNegate elem)))))]
    [x (set (set x))]))

(: dnf->string (-> Type-DNF String))
(define (dnf->string dnf)
  (string-join
   (for/list ([clause dnf]) : (Listof String)
     (string-append "("
                    (string-join
                     (for/list ([type clause]) : (Listof String)
                       (type->string type))
                     " ⋀ ")
                    ")"))
   " ⋁ "))

#;(let ([T (TNegate (TNegate (TIntersect (TUnion (TBytes 3) (TBytes 5)) (TNegate (TBytes 3)))))])
    (displayln (type->string T))
    (displayln (dnf->string (type->dnf T))))
                                        
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
    [(TFail t) `(and ,(string->symbol (format "~a-fail-~a" var-name t))
                     ,(string->symbol (format "~a-fail" var-name)))]
    [(TUnion x y) `(or ,(type->sat x var-name)
                       ,(type->sat y var-name))]
    [(TIntersect x y) `(and ,(type->sat x var-name)
                            ,(type->sat y var-name))]
    [(TNegate x) `(not ,(type->sat x var-name))]
    [type
     `(and (not ,(string->symbol (format "~a-fail" var-name)))
           ,(match type
              [(TBin) (string->symbol (format "~a-bin" var-name))]
              [(TNat) ; a Nat is a union of a Bin and a NonBinNat
               `(or ,(string->symbol (format "~a-bin" var-name))
                    ,(string->symbol (format "~a-nbin" var-name)))]
              [(TAny) ; always true
               `(not ,(string->symbol (format "~a-fail" var-name)))]
              [(TNone) ; always false
               #f]
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
                                 subelems)]))]))

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
  (time (subtype-of? (TNat) (TAny)))
  )

#|

def f<T>(x: [T * 6]) -> T = x[0]

f([1, 2, 3, 4, 5, 6])

find T such that [Nat * 6] <: [T * 6]

|#