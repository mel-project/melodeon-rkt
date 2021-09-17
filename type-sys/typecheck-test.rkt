#lang racket

(require rackunit
         "../grammar/parser.rkt"
         "../asts/typed-ast.rkt"
         "types.rkt"
         "typecheck.rkt"
         compatibility/defmacro)

;; TODO: these tests are incredibly fragile to changes in the typechecker/transformer of trivial consequence. Should we just test only the type then? 

;; Macros, so that the failing tests have the right line numbers instead of all failing in prgrm-eq?
(define-macro (prgrm-eq? str $prgm)
  `(check-equal?
    (let ([@program (melo-parse-port (open-input-string ,str))])
      (@-transform @program))
    ,$prgm))

(define-macro (prgrm-type-eq? str type)
  `(check-equal?
    ($-Ast-type ($program-expr (let ([@program (melo-parse-port (open-input-string ,str))])
                                 (@-transform @program))))
    ,type))

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
      ($slice
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

#;(prgrm-eq?
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

(prgrm-type-eq?
  "
  def foo(x: Nat) = x
  struct X { x : Nat }
  struct Y { x : Nat }
  ---
[3 * (
  let x = X { 2 } in
  let y = Y { 3 } in y.x)][0] - 1
  "
  (TNat))

(prgrm-type-eq?
 "def roundtrip<T>(x: T) = getfirst(dup(x))
def dup<T>(x: T) = [x, x]
def getfirst<T>(x : [T, T]) = x[0]
---
roundtrip(1)
" (TNat))
 
