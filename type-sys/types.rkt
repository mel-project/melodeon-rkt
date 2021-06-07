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

(define TVectorU? (make-predicate (U TVectorof TVector)))


;; Type
(define-type Type (U TNat
                     TBin
                     TAny
                     TVector
                     TVectorof))

(define Type? (make-predicate Type))