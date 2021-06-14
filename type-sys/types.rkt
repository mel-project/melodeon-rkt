#lang typed/racket
(provide (all-defined-out))

;; Base types 
(struct TNat () #:prefab)
(struct TBin () #:prefab)
(struct TAny () #:prefab)

;; Composite types
(struct TVector ((lst : (Listof Type))) #:prefab)
(struct TVectorof ((inner : Type)
                   (count : Nonnegative-Integer)
                   ) #:prefab)
(struct TBytes ((count : Nonnegative-Integer)
                ) #:prefab)

(define TVectorU? (make-predicate (U TVectorof TVector)))

(struct TUnion ((x : Type)
                (y : Type)) #:prefab)


;; Type
(define-type Type (U TNat
                     TBin
                     TAny
                     TVector
                     TVectorof
                     TUnion
                     TBytes))

(define Type? (make-predicate Type))

;; Get the string representation of a type
(: type->string (-> Type String))
(define (type->string type)
  (match type
    [(TNat) "Nat"]
    [(TBin) "Bin"]
    [(TAny) "Any"]
    [(TVector lst) (define inner-names (map type->string lst))
                   (string-append "["
                                  (string-join inner-names ", ")
                                  "]")]
    [(TVectorof t n) (string-append "["
                                    (type->string t)
                                    " * "
                                    (number->string n)
                                    "]")]
    [(TUnion l r) (string-append
                   (type->string l)
                   " | "
                   (type->string r))]
    [(TBytes n) (format "Bytes[~a]" n)]))