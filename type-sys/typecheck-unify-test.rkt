#lang racket
(require "typecheck-unify.rkt"
         "types.rkt")

(require rackunit)

(check-equal?
  (type-append (TVectorof (TNat) 2)
               (TVectorof (TNat) 3))
  (TUnion (TNone) (TVector (list (TAny) (TAny) (TAny) (TAny) (TAny)))))

(check-equal?
  (type-append (TVectorof (TNat) '(+ 2 n))
               (TVectorof (TNat) '(+ 3 n)))
  (TUnion (TNone) (TVectorof (TNat) '(+ 5 (* 2 n)))))
