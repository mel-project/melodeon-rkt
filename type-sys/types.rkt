#lang typed/racket
(provide (all-defined-out))

;; Base types 
(struct TNat () #:transparent)
(struct TAny () #:transparent)

;; Composite types
(struct TVector ((lst : (Listof Type))) #:transparent)
(struct TVectorof ((inner : Type)
                   (count : Nonnegative-Integer)
                   ) #:transparent)
;; A vector where the length is unknown, but all values of are the given type
(struct TDynVectorof ((inner : Type)) #:transparent)
(struct TBytes ((count : Nonnegative-Integer)
                ) #:transparent)

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
                     TAny
                     TTagged
                     TDynVectorof
                     TVector
                     TVectorof
                     TUnion
                     TIntersect
                     TNone
                     TBytes
                     TVar))

(define Type? (make-predicate Type))

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
    [(TVectorof t n) (string-append "["
                                    (type->string t)
                                    " * "
                                    (number->string n)
                                    "]")]
    [(TUnion l r) (format "(~a | ~a)"
                          (type->string l)
                          (type->string r))]
    [(TIntersect l r) (format "(~a & ~a)"
                              (type->string l)
                              (type->string r))]
    [(TBytes n) (format "Bytes[~a]" n)]))

