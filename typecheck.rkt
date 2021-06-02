#lang racket
(require "parser.rkt")

;; typechecks an @-ast
;; @-ast -> Type
(define (@-ast->type @-ast (types (hash)))
  ;; context-capturing helpers
  (define (lookup-binding sym)
    (match (hash-ref types sym #f)
      [#f (error 'lookup-binding
                 "[~a] undefined variable ~a"
           (context->string (get-context @-ast))
           sym)]
      [val val]))
  (define (assert-type @-ast type)
    (define real-type (@-ast->type @-ast types))
    (unless (equal? real-type type)
      (error 'assert-type
             "[~a] expected type ~a, got type ~a"
             (context->string (get-context @-ast))
             type
             real-type)))
           
  
  (match @-ast
    [`(,(? (lambda(op) (member op '(@+ @- @* @/)))) ,x ,y)
     (assert-type x 'Nat)
     (assert-type y 'Nat)
     'Nat]
    [`(@let (,val ,expr) ,body)
     (define expr-type (@-ast->type expr types))
     (@-ast->type body (hash-set types val expr-type))]
    [`(@lit-num ,_) 'Nat]
    [`(@var ,variable) (lookup-binding variable)]))

(module+ main
  (@-ast->type
   (melo-parse-port (open-input-string "
let x = 123 in
let y = 456 in
    x + (let y = y*y*123545 in y * x + y)
"))))