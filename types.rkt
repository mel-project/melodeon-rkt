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

(: dectx (-> (MaybeCtx @-Ast) @-Ast))
(define (dectx wc)
  (cond
    [(with-context? wc) (dectx (with-context-matter wc))]
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
     (with-context @-Ast)))

(define-type @-Binop (U '@+ '@- '@* '@/))

;; Mil format
(define-type Mil
  (U (List 'let (List Symbol Mil) Mil)
     (List (U '+ '- '* '/) Mil Mil)
     (Vectorof Mil)
     Integer
     Symbol))