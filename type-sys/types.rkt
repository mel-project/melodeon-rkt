#lang typed/racket
(provide (all-defined-out))

;; Base types 
(struct TNat () #:transparent)
(struct TBin () #:transparent)
(struct TAny () #:transparent)

;; Composite types
(struct TVector ((lst : (Listof Type))) #:transparent)
(struct TVectorof ((inner : Type)
                   (count : Nonnegative-Integer)
                   ) #:transparent)
;; A vector where the final length is unknown, but it's at least the given
(struct TVectorEtc ((list : (Listof Type))) #:transparent)
(struct TBytes ((count : Nonnegative-Integer)
                ) #:transparent)

(define TVectorU? (make-predicate (U TVectorof TVector)))

;; "Set-theoretical" type combinators
(struct TUnion ((x : Type)
                (y : Type)) #:transparent)
(struct TIntersect ((x : Type)
                    (y : Type)) #:transparent)
(struct TNegate ((x : Type)) #:transparent)

(struct TNone () #:transparent)

; Represents a custom defined product type that is distinct by its name
(struct TTagged ((tag : Symbol)
                 (lst : (Listof Type))) #:transparent)


;; Type
(define-type Type (U TNat
                     TBin
                     TAny
                     TTagged
                     TVector
                     TVectorof
                     TUnion
                     TIntersect
                     TNone
                     TVectorEtc
                     TNegate
                     TBytes))

(define Type? (make-predicate Type))

;; Get the string representation of a type
(: type->string (-> Type String))
(define (type->string type)
  (match type
    [(TNat) "Nat"]
    [(TBin) "Bin"]
    [(TAny) "Any"]
    [(TTagged tag types) (define type-strs (map type->string types))
                           (string-append "["
                                          (symbol->string tag)
                                          (string-join type-strs ", ")
                                          "]")]
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
    [(TNegate v) (format "~~ ~a" (type->string v))]
    [(TBytes n) (format "Bytes[~a]" n)]))

