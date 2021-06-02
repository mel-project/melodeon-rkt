#lang typed/racket
(require "types.rkt"
         "parser.rkt")
(provide Type
         @-ast->type)

(define-type Type
  (U 'Nat
     ; "tuple" types
     (Pair 'Vector (Listof Type))))

;; typechecks an @-ast
(: @-ast->type (-> @-Ast (HashTable Symbol Type) Type))
(define (@-ast->type @-ast scope)
  ;; context-capturing helpers
  (: lookup-binding (-> Symbol Type))
  (define (lookup-binding sym)
    (match (hash-ref scope sym #f)
      [#f (error 'lookup-binding
                 "[~a] undefined variable ~a"
           (context->string (get-context @-ast))
           sym)]
      [val (cast val Type)]))
  
  (: assert-type (-> @-Ast Type Void))
  (define (assert-type @-ast type)
    (define real-type (@-ast->type @-ast scope))
    (unless (equal? real-type type)
      (error 'assert-type
             "[~a] expected type ~a, got type ~a"
             (context->string (get-context @-ast))
             type
             real-type)))
           
  
  (match (dectx @-ast)
    [`(,(? (lambda(op) (member op '(@+ @- @* @/)))) ,x ,y)
     (assert-type x 'Nat)
     (assert-type y 'Nat)
     'Nat]
    [`(@let (,val ,expr) ,body)
     (define expr-type (@-ast->type expr scope))
     (@-ast->type body (hash-set scope val expr-type))]
    [`(@lit-num ,_) 'Nat]
    [`(@var ,variable) (lookup-binding variable)]
    [`(@lit-vec ,vars) `(Vector . ,(map (λ ((x : @-Ast)) (@-ast->type x scope)) vars))]))

(module+ main
  (@-ast->type
   (melo-parse-port (open-input-string "
[1, 2, [3,4], [5 * (let x = 2 in x + 6)]]
"))
   (hash)))