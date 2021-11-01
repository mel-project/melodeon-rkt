#lang typed/racket
(provide (all-defined-out))

;; Base types 
(struct TNat () #:transparent)
(struct TNatRange ((start : (Option Const-Expr))
                   (end : (Option Const-Expr)))
  #:transparent)
(struct TAny () #:transparent)

;; Constant expressions on generics
(define-type Op (U '+ '- '* '/ '^))
(define-type Const-Expr
  ;(U (List Op (Listof Const-Expr))
  (U (List Op Const-Expr Const-Expr)
     Symbol
     Integer))

(define const-expr? (make-predicate Const-Expr))

;(const-expr? '(+ n 2))
;(const-expr? '(* (+ x 1) (- x 1)))

;; Composite types
(struct TVector ((lst : (Listof Type))) #:transparent)
(struct TVectorof ((inner : Type)
                   (count : Const-Expr)
                   ) #:transparent)
;; A vector where the length is unknown, but all values of are the given type
(struct TDynVectorof ((inner : Type)) #:transparent)
(struct TBytes ((count : Const-Expr)
                ) #:transparent)
;; A byte string of unknown length
(struct TDynBytes () #:transparent)

(define TVectorU? (make-predicate (U TVectorof TVector)))

;; "Set-theoretical" type combinators
(struct TUnion ((x : Type)
                (y : Type)) #:transparent)
(struct TIntersect ((x : Type)
                    (y : Type)) #:transparent)

;; Represents a type variable
(struct TVar ((label : Symbol)) #:transparent)

(struct TNone () #:transparent)

;; Represents a unique "fail" type that contains no values, cannot be constructed, etc. This is used internally to indicate a "bad" type.
(struct TFail ((label : Symbol)) #:transparent)

;; Generates a TFail corresponding to the given object.
(: gen-tfail (-> Any TFail))
(define gen-tfail
  (let ([cache : (HashTable Any TFail) (make-hash)])
    (lambda ((val : Any))
      (cond
        [(hash-has-key? cache val) (hash-ref cache val)]
        [else (define obj (TFail (gensym)))
              (hash-set! cache val obj)
              obj]))))

; Represents a custom defined product type that is distinct by its name
(struct TTagged ((tag : Symbol)
                 (lst : (Listof Type))) #:transparent)


;; Type
(define-type Type (U TNat
                     TNatRange
                     TAny
                     TTagged
                     TDynVectorof
                     TDynBytes
                     TVector
                     TVectorof
                     TUnion
                     TIntersect
                     TNone
                     TBytes
                     TVar))

(define Type? (make-predicate Type))

(: const-expr->string (-> Const-Expr String))
(define (const-expr->string e)
  (match e
    [`(,op ,e1 ,e2)
      (format "~a ~a ~a" e1 op e2)]
    [(? symbol? s) (symbol->string s)]
    [(var x) (format "~a" x)]))

;; Get the string representation of a type
(: type->string (-> Type String))
(define (type->string type)
  (match type
    [(TNone) "None"]
    [(TNat) "Nat"]
    [(TAny) "Any"]
    [(TVar s) (format "'~a" s)]
    [(TFail s) (format "Fail[~a]" s)]
    [(TTagged tag types) (define type-strs (map type->string types))
                         (string-append 
                                        (symbol->string tag)
                                        "{"
                                        (string-join type-strs ", ")
                                        "}")]
    [(TVector lst) (define inner-names (map type->string lst))
                   (string-append "["
                                  (string-join inner-names ", ")
                                  "]")]
    [(TVectorof t e) (string-append "["
                                    (type->string t)
                                    " * "
                                    (const-expr->string e)
                                    "]")]
    [(TDynVectorof t) (format "[~a *]" (type->string t))]
    [(TUnion l r) (format "(~a | ~a)"
                          (type->string l)
                          (type->string r))]
    [(TIntersect l r) (format "(~a & ~a)"
                              (type->string l)
                              (type->string r))]
    [(TBytes n) (format "%[~a]" n)]
    [(TDynBytes) "%[]"]
    [(TNatRange a b) (format "{~a..~a}" a b)]))

