#lang racket
(require "type-bag.rkt"
         "types.rkt")

(require rackunit)

;; trivial subtraction test
(check-equal? (bag-subtract (type->bag (TUnion (TNat) (TBytes 1)))
                            (type->bag (TNat)))
              (type->bag (TBytes 1)))

(check-equal? (bag-subtract (type->bag (TUnion (TUnion (TNat) (TBytes 1))
                                               (TBytes 5)))
                            (type->bag (TBytes 3)))
              (type->bag (TUnion (TUnion (TNat) (TBytes 1))
                                 (TBytes 5))))

(bag-subtract (type->bag (TUnion (TVectorof (TNat) 2)
                                 (TVectorof (TUnion (TNat)
                                                    (TBytes 3)) 3)))
              (type->bag (TVectorof (TAny) 3)))