#lang typed/racket
(provide (all-defined-out))

;; Primary intermediate representation
(define-type @-Ast
  (U (List '@let (List Symbol @-Ast) @-Ast)
     (List @-Binop @-Ast @-Ast)
     (List '@lit-num Integer)
     (List '@lit-vec (Listof @-Ast))
     (List '@var Symbol)))

(define-type @-Binop (U '@+ '@- '@* '@/))

;; Mil format
(define-type Mil
  (U (List 'let (List Symbol Mil) Mil)
     (List (U '+ '- '* '/) Mil Mil)
     Integer
     Symbol))