#lang typed/racket
(provide (all-defined-out))
(require/typed parser-tools/lex
               [#:struct position ([offset : Integer] [line : Integer] [col : Integer ])])
;(provide position)

;; Context-tracking
(define-type (MaybeCtx a)
  (U a
     (with-context a)))

(struct (a) with-context
  ((ctx : context)
   (matter : a))
  #:transparent)

;; NOTE: due to TR limitation, this function cannot be fully generic :(
(: dectx (-> @-Ast @-Ast))
(define (dectx wc)
  (cond
    [(with-context? wc) (dectx (with-context-matter wc))]
    [else wc]))


(: dectx* (-> Any Any))
(define (dectx* wc)
  (cond
    [(list? wc) (map dectx* wc)]
    [(with-context? wc) (dectx* (with-context-matter wc))]
    [else wc]))

(: get-context (-> (MaybeCtx @-Ast) (Option context)))
(define (get-context wc)
  (cond
    [(with-context? wc) (with-context-ctx wc)]
    [else #f]))

(struct context
  ((filename : String)
   (start-pos : position)
   (end-pos : position)))

;; Primary intermediate representation
(define-type @-Ast
  (U (List '@let (List Symbol @-Ast) @-Ast)
     (List @-Binop @-Ast @-Ast)
     (List '@lit-num Integer)
     (List '@lit-vec (Listof @-Ast))
     (List '@var Symbol)
     (List '@program (Listof Definition) @-Ast)
     (List '@apply Symbol (Listof @-Ast))
     (List '@block (Listof @-Ast))
     (with-context @-Ast)))

(define-type Definition
  (U (List '@def-var Symbol @-Ast)
     (List '@def-fun Symbol
           (Listof (List Symbol Type-Expr))
           (Option Type-Expr)
           @-Ast)))

(define-type Type-Expr
  (U (List '@type-var Symbol)
     (List '@type-vec (Listof Type-Expr))))

(define-type @-Binop (U '@+ '@- '@* '@/))