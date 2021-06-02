#lang racket

;; @-ast->mil: @-Ast -> Mil
(define (@-ast->mil @-ast)
  (define (strip-@ @-sym)
    (string->symbol (substring (symbol->string @-sym) 2)))

  (match @-ast
    ;; let bindings 
    [`(@let (,var-name ,var-value) ,body)
     `(let (,var-name ,(@-ast->mil var-value)) ,(@-ast->mil body))]
    ;; binary ops
    [`(,(? (lambda(op) (member op '(@+ @- @* @/))) op) ,x ,y) `(,(strip-@ op) ,(@-ast->mil x)
                                                                              ,(@-ast->mil y))]
    [`(@var ,(? symbol? varname)) varname]
    [`(@lit-num ,(? exact-integer? number)) number]
    [other (error "invalid @-ast" other)]))