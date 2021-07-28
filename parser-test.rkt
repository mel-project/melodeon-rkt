#lang racket/base

(require rackunit
         "common.rkt"
         "parser.rkt")

(define (parse-eq? str prgm)
  (check-equal?
    (dectx* (melo-parse-port (open-input-string str)))
    prgm))


(parse-eq?
  "0"
  '(@program () (@lit-num 0)))

(parse-eq?
  "
  def foo(x: Nat) = x*2
  ---
  foo(4)
  "
  '(@program
    ((@def-fun foo ((x (@type-var Nat))) #f (@* (@var x) (@lit-num 2))))
    (@apply foo ((@lit-num 4)))))

(parse-eq?
  "[x*2 for x in [1, 2, 3]]"
  '(@program
    ()
    (@for
     (@* (@var x) (@lit-num 2))
     x
     (@lit-vec ((@lit-num 1) (@lit-num 2) (@lit-num 3))))))
