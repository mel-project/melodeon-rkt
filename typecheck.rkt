#lang typed/racket
(require "types.rkt"
         "parser.rkt")
(provide Type
         @-ast->type)

(define-type Type
  (U ValType
     (List 'Function (Listof ValType) ValType)))

(define-type ValType
  (U Symbol
     ; "tuple" types
     (Pair 'Vector (Listof ValType))
     ))

;; typechecks an @-ast
(: @-ast->type (-> @-Ast (HashTable Symbol Type) ValType))
(define (@-ast->type @-ast scope)
  ;; context-capturing helpers
  (: lookup-binding (-> Symbol Type))
  (define (lookup-binding sym)
    (match (hash-ref scope sym #f)
      [#f (error 'lookup-binding
                 "[~a] undefined variable: ~a"
           (context->string (get-context @-ast))
           sym)]
      [val (cast val Type)]))

  (: lookup-var-binding (-> Symbol ValType))
  (define (lookup-var-binding sym)
    (define res (lookup-binding sym))
    (if ((make-predicate ValType) res)
        res
        (error 'lookup-var-binding
                 "[~a] non-variable type used out of context: ~a"
           (context->string (get-context @-ast))
           sym)))
  
  (: assert-type (-> @-Ast Type Void))
  (define (assert-type @-ast type)
    (define real-type (@-ast->type @-ast scope))
    (unless (equal? real-type type)
      (error 'assert-type
             "[~a] expected type ~a, got type ~a"
             (context->string (get-context @-ast))
             type
             real-type)))

  (: resolve-type (-> Type-Expr ValType))
  (define (resolve-type texpr)
    (match texpr
      [`(@type-var ,var) var]
      [`(@type-vec ,vec) `(Vector . ,(map resolve-type vec))]))
           
  
  (match (dectx @-ast)
    [`(@program ,definitions ,body)
     (define new-mapping (foldl (λ ((binding : Definition) (accum : (HashTable Symbol Type)))
                                  (match binding
                                    [`(@def-var ,var ,expr) (hash-set accum var (@-ast->type expr accum))]
                                    [`(@def-fun ,fun ,args ,rettype ,expr)
                                     (define inner-scope
                                       (foldl (λ ((pair : (List Symbol Type-Expr)) (accum : (HashTable Symbol Type)))
                                                (hash-set accum (first pair) (resolve-type (second pair))))
                                              accum
                                              args))
                                     (hash-set accum fun
                                               `(Function ,(map resolve-type (map (inst second Symbol Type-Expr Any)
                                                                                  args))
                                                          ,(@-ast->type expr inner-scope)))]))
                                scope
                                definitions))
     (@-ast->type body new-mapping)]
    [`(,(? (lambda(op) (member op '(@+ @- @* @/)))) ,x ,y)
     (assert-type x 'Nat)
     (assert-type y 'Nat)
     'Nat]
    [`(@let (,val ,expr) ,body)
     (define expr-type (@-ast->type expr scope))
     (@-ast->type body (hash-set scope val expr-type))]
    [`(@lit-num ,_) 'Nat]
    [`(@var ,variable) (lookup-var-binding variable)]
    [`(@lit-vec ,vars) `(Vector . ,(map (λ ((x : @-Ast)) (@-ast->type x scope)) vars))]
    [`(@apply ,fun ,args)
     (match (lookup-binding fun)
       [`(Function ,arg-types ,result)
        (unless (equal? (length arg-types) (length args))
          (error '@-ast->type "[~a] calling function ~a with ~a arguments instead of the expected ~a"
                 (context->string (get-context @-ast))
                 fun
                 (length args)
                 (length arg-types)))
        (for ([arg-expr (in-list args)]
              [arg-type (in-list arg-types)])
          (assert-type arg-expr arg-type))
        result]
       [_ (error '@-ast->type "[~a] undefined function ~a"
             (context->string (get-context @-ast)) fun)])]
    ))

(module+ main
  (@-ast->type
   (melo-parse-port (open-input-string "
def @f (x:Nat y:Nat) =
  [x+y y*x]
def @g (x:Nat) =
  [@f(x x) x @f(x x)]

@g(123)
"))
   (hash)))