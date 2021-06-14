#lang racket
(require racklog
         racket/trace)
(require "../common.rkt"
         "types.rkt")
(provide (all-defined-out))
(require memo)
(use-occurs-check? #f)



;; Prolog-style type resolver

;; find all the common supertypes between the two types, sorted by subtyping relation
#|
(define (common-supertypes t1 t2)
  (sort
   (map cdr
        (append*
         (remove-duplicates
          (%find-all (what)
                     (%and (%subtype-of (rewrite-vectorof t1) what)
                           (%subtype-of (rewrite-vectorof t2) what))))))
   type-smaller))

(define (common-supertypes/raw t1 t2)
  (%which (what)
          (%and (%subtype-of (rewrite-vectorof t1) what)
                (%subtype-of (rewrite-vectorof t2) what))))|#

;; a total ordering of types. break types that are subtypes of each other by lexicographic order
(define/memoize (type-smaller t1 t2)
  (cond
    [(and (subtype-of? t1 t2)
          (subtype-of? t2 t1))
     (string<? (format "~a" t1)
               (format "~a" t2))]
    [else
     (subtype-of? t1 t2)]))

;; is t1 a subtype of t2
(define/memoize (subtype-of? t1 t2)
  (and (%which ()
               (%subtype-of (rewrite-vectorof t1)
                            (rewrite-vectorof t2)))
       #t))

(define %subtype-of
  (%rel (x y z x-count y-count x-rest y-rest x-expanded y-expanded)
    ; direct subtypes
    [(x y) (%imm-subtype-of x y)]
    ; [Subtype, Subtype ..] <: [Supertype, Supertype ..]
    [(#s(TVector ()) #s(TVector ()))]
    [(`#s(TVector ,x)
      `#s(TVector ,y))
     (%andmap %subtype-of
              x
              y)]
    ; union types
    [(x `#s(TUnion ,y ,z)) (%or (%subtype-of x y)
                                (%subtype-of x z))]
    ; or there exists some z which x is a subtype of, which is a subtype of y
    [(x y) (%and
            (%imm-subtype-of x z)
            (%subtype-of z y))]
    ; every type is a subtype of itself and of any
    [(x (TAny))]
    [(x y) (%= x y)]
    ))

;(trace %subtype-of)

(define %imm-subtype-of
  (%rel (x y)
    [((TBin) (TNat))]
    [((TNat) (TAny))]))

(define (rewrite-vectorof v)
  (match v
    [(TVectorof t n)
     (TVector (make-list n (rewrite-vectorof t)))]
    [(TVector lst)
     (TVector (map rewrite-vectorof lst))]
    [(TUnion x y)
     (TUnion (rewrite-vectorof x)
             (rewrite-vectorof y))]
    [x x]))

;; find all 

(module+ test
  
  (define x (TVector (list (TVector (list (TNat) (TNat)))
                           (TVector (list (TNat) (TNat))))))
  (define y (TVectorof (TVector (list (TAny) (TAny))) 2))
  (define z (TVector (list (TVector (list (TAny) (TAny)))
                           (TVector (list (TAny) (TAny))))))

  (subtype-of? x (TUnion (TNat) y))
  
  )