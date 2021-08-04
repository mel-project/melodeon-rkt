#lang racket/base

(require rackunit
         "../common.rkt"
         "../parser.rkt"
         "../typed-ast.rkt"
         "types.rkt"
         "typecheck.rkt")

(define (prgrm-eq? str $prgm)
  (check-equal?
    (let ([@program (melo-parse-port (open-input-string str))])
      (@-transform @program))
    $prgm))

(prgrm-eq?
  "0"
  ($program '() '() ($-Ast (TBin) ($lit-num 0))))

(prgrm-eq?
  "
  def foo(x: Nat) = x
  struct X { x : Nat }
  struct Y { x : Nat }
  ---
  let x = X { 2 } in
  let y = Y { 3 } in foo(0)
  "
  ($program
   (list ($fndef 'foo (list (list 'x (TNat))) ($-Ast (TNat) ($var 'x))))
   '()
   ($-Ast
    (TNat)
    ($let
     'x
     ($-Ast
      (TVector (list (TNat) (TNat)))
      ($lit-vec (list ($-Ast (TNat) ($lit-num 88)) ($-Ast (TNat) ($lit-num 2)))))
     ($-Ast
      (TNat)
      ($let
       'y
       ($-Ast
        (TVector (list (TNat) (TNat)))
        ($lit-vec
         (list ($-Ast (TNat) ($lit-num 89)) ($-Ast (TNat) ($lit-num 3)))))
       ($-Ast (TNat) ($apply 'foo (list ($-Ast (TBin) ($lit-num 0)))))))))))
