#lang racket/base

(require rackunit
         "../parser.rkt"
         "../typed-ast.rkt"
         "types.rkt"
         "typecheck.rkt")

(define (prgrm-eq? str $prgm)
  (check-equal?
    (let ([@program (melo-parse-port (open-input-string str))])
      (@-transform @program))
     $prgm))
    ;  (format "~a" (@-transform @program)))
    ;(format "~a" $prgm)))

(prgrm-eq?
  "0"
  ($program '() '() ($-Ast (TNat) ($lit-num 0))))

(prgrm-eq?
  "let v = [1,2,3,4] in v[0..2]"
  ($program
   '()
   '()
   ($-Ast
    (TNat)
    ($let
     'v
     ($-Ast
      (TVector (list (TNat) (TNat) (TNat) (TNat)))
      ($lit-vec
       (list
        ($-Ast (TNat) ($lit-num 1))
        ($-Ast (TNat) ($lit-num 2))
        ($-Ast (TNat) ($lit-num 3))
        ($-Ast (TNat) ($lit-num 4)))))
     ($-Ast
      (TNat)
      ($range
       ($-Ast (TVector (list (TNat) (TNat) (TNat) (TNat))) ($var 'v))
       ($-Ast (TNat) ($lit-num 0))
       ($-Ast (TNat) ($lit-num 2))))))))

(prgrm-eq?
  "1 | 2 & 3 ^ 4 << 5"
    ($program
   '()
   '()
   ($-Ast
    (TNat)
    ($bin
     'shl
     ($-Ast
      (TNat)
      ($bin
       'xor
       ($-Ast
        (TNat)
        ($bin
         'or
         ($-Ast (TNat) ($lit-num 1))
         ($-Ast
          (TNat)
          ($bin 'and ($-Ast (TNat) ($lit-num 2)) ($-Ast (TNat) ($lit-num 3))))))
       ($-Ast (TNat) ($lit-num 4))))
     ($-Ast (TNat) ($lit-num 5))))))

(prgrm-eq?
  "
  struct Y { x : Nat }
  ---
  let y = Y { 3 } in y.x
  "
  ($program
   (list
    ($fndef
     'Y-x
     (list (list '@x (TTagged 'Y (list (TNat)))))
     ($-Ast
      (TNat)
      ($index
       ($-Ast (TTagged 'Y (list (TNat))) ($var '@x))
       ($-Ast (TNat) ($lit-num 1))))))
   '()
   ($-Ast
    (TNat)
    ($let
     'y
     ($-Ast
      (TTagged 'Y (list (TNat)))
      ($lit-vec (list ($-Ast (TNat) ($lit-num 89)) ($-Ast (TNat) ($lit-num 3)))))
     ($-Ast
      (TNat)
      ($apply 'Y-x (list ($-Ast (TTagged 'Y (list (TNat))) ($var 'y)))))))))

(prgrm-eq?
  "
  def foo(x: Nat) = x
  struct X { x : Nat }
  struct Y { x : Nat }
  ---
  let x = X { 2 } in
  let y = Y { 3 } in y.x
  "
  ($program
   (list
    ($fndef
     'X-x
     (list (list '@x (TTagged 'X (list (TNat)))))
     ($-Ast
      (TNat)
      ($index
       ($-Ast (TTagged 'X (list (TNat))) ($var '@x))
       ($-Ast (TNat) ($lit-num 1)))))
    ($fndef
     'Y-x
     (list (list '@x (TTagged 'Y (list (TNat)))))
     ($-Ast
      (TNat)
      ($index
       ($-Ast (TTagged 'Y (list (TNat))) ($var '@x))
       ($-Ast (TNat) ($lit-num 1)))))
    ($fndef 'foo (list (list 'x (TNat))) ($-Ast (TNat) ($var 'x))))
   '()
   ($-Ast
    (TNat)
    ($let
     'x
     ($-Ast
      (TTagged 'X (list (TNat)))
      ($lit-vec (list ($-Ast (TNat) ($lit-num 88)) ($-Ast (TNat) ($lit-num 2)))))
     ($-Ast
      (TNat)
      ($let
       'y
       ($-Ast
        (TTagged 'Y (list (TNat)))
        ($lit-vec
         (list ($-Ast (TNat) ($lit-num 89)) ($-Ast (TNat) ($lit-num 3)))))
       ($-Ast
        (TNat)
        ($apply 'Y-x (list ($-Ast (TTagged 'Y (list (TNat))) ($var
                                                               'y)))))))))))
