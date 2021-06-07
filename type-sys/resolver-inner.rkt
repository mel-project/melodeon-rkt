#lang racket
(require racklog)
(require "../common.rkt"
         "types.rkt")
(provide (all-defined-out))

(use-occurs-check? #t)



;; Prolog-style type resolver

;; find all the common supertypes between the two types, sorted by subtyping relation
(define (common-supertypes t1 t2)
  (sort
   (map cdr
        (append*
         (remove-duplicates
          (%find-all (what)
                     (%and (%subtype-of t1 what)
                           (%subtype-of t2 what))))))
   type-smaller))

;; a total ordering of types. break types that are subtypes of each other by lexicographic order
(define (type-smaller t1 t2)
  (cond
    [(and (subtype-of? t1 t2)
          (subtype-of? t2 t1))
     (string<? (format "~a" t1)
               (format "~a" t2))]
    [else
     (subtype-of? t1 t2)]))

;; is t1 a subtype of t2
(define (subtype-of? t1 t2)
  (and (%which ()
               (%subtype-of t1 t2))
       #t))

(define %subtype-of
  (%rel (x y z x-count y-count x-rest y-rest)
    ; every type is a subtype of itself and of any
    [(x y) (%= x y)]
    [(x y) (%equivtype x y)]
    [(x (TAny))]
    ; direct subtypes
    [(x y) (%imm-subtype-of x y)]
    ; [Subtype * n] <: [Supertype * n]
    [(`#s(TVectorof ,x ,x-count)
      `#s(TVectorof ,y ,y-count))
     (%and (%= x-count
               y-count)
           (%subtype-of x y))]
    ; [Subtype, Subtype ..] <: [Supertype, Supertype ..]
    [(#s(TVector ()) #s(TVector ()))]
    [(`#s(TVector ,(cons x x-rest))
      `#s(TVector ,(cons y y-rest)))
     (%and (%subtype-of x y)
           (%subtype-of `#s(TVector ,x-rest)
                        `#s(TVector ,y-rest)))]
    ; or there exists some z which x is a subtype of, which is a subtype of y
    [(x y) (%and (%/= x z)
                 (%subtype-of x z)
                 (%subtype-of z y))]
    ))

(define %imm-subtype-of
  (%rel (x y)
    [((TBin) (TNat))]))

(define %equivtype
  (%rel (x y)
    [(x y) (%= x y)]
    [(x y) (%rewrite-to x y)]
    [(x y) (%rewrite-to y x)]))

(define %rewrite-to
  (%rel (x y xx-car xx-cdr x-count x-count-sub1)
    [(`#s(TVectorof ,x 0)
      `#s(TVector ()))]
    [(`#s(TVectorof ,x ,x-count)
      `#s(TVector ,(cons xx-car xx-cdr)))
     (%is x-count-sub1 (sub1 x-count))
     (%and (%> x-count 0)
           (%equivtype x xx-car)
           (%rewrite-to `#s(TVectorof ,x ,x-count-sub1)
                        `#s(TVector ,xx-cdr)))]))

;; find all 

(module+ test
  (require test-engine/racket-tests)
  (check-expect (%which ()
                        (%subtype-of (TVectorof (TAny) 1)
                                     (TVectorof (TBin) 1)
                                     ))
                #f)

  ;; common supertypes of [[Any, Any] * 2] and [[Any, Any], [Any, Any]]:
  ;; - [[Any, Any], [Any, Any]]
  ;; - [[Any, Any] * 2]
  ;; - Any
  (check-expect (common-supertypes
                 (TVectorof (TVector (list (TAny) (TAny))) 2)
                 (TVector (list (TVector (list (TAny) (TAny)))
                                (TVector (list (TAny) (TAny))))))
                '(#s(TVector (#s(TVector (#s(TAny) #s(TAny))) #s(TVector (#s(TAny) #s(TAny)))))
                  #s(TVectorof #s(TVector (#s(TAny) #s(TAny))) 2)
                  #s(TAny)))

  (common-supertypes
   (TVectorof (TVector (list (TNat) (TNat))) 2)
   (TVector (list (TVector (list (TNat) (TNat)))
                  (TVector (list (TNat) (TNat))))))

  (define x (TVector (list (TVector (list (TNat) (TNat)))
                           (TVector (list (TNat) (TNat))))))
  (define y (TVectorof (TVector (list (TAny) (TAny))) 2))
  (define z (TVector (list (TVector (list (TAny) (TAny)))
                           (TVector (list (TAny) (TAny))))))

  (subtype-of? x z)
  (subtype-of? z y)
  (subtype-of? x y)

  (%which ()
          (%and (%/= x z)
                (%subtype-of x z)
                (%subtype-of z y)))
  )