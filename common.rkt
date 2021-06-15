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

;; Contextful error handling
(: current-context (Parameter (Option context)))
(define current-context (make-parameter #f))

(: context-of (-> @-Ast (Option context)))
(define (context-of ast)
  (or (get-context ast) (current-context)))

(: context-error (-> String Any * Nothing))
(define (context-error format-string . rst)
  (error 'typecheck
         "[~a] ~a"
         (context->string (current-context))
         (apply format `(,format-string . ,rst))))


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

(: context->string (-> (Option context) String))
(define (context->string ctx)
  (if ctx
      (format "~a, ~a:~a-~a:~a"
              (context-filename ctx)
              (position-line (context-start-pos ctx))
              (position-col (context-start-pos ctx))
              (position-line (context-end-pos ctx))
              (position-col (context-end-pos ctx)))
      "NO CONTEXT"))
  

;; Primary intermediate representation
(define-type @-Ast
  (U (List '@let (List Symbol @-Ast) @-Ast)
     (List @-Binop @-Ast @-Ast)
     (List '@lit-num Nonnegative-Integer)
     (List '@lit-vec (Listof @-Ast))
     (List '@var Symbol)
     (List '@program (Listof Definition) @-Ast)
     (List '@apply Symbol (Listof @-Ast))
     (List '@block (Listof @-Ast))
     (List '@index @-Ast @-Ast)
     (List '@update @-Ast @-Ast @-Ast)
     (List '@unsafe-cast @-Ast Type-Expr)
     (List '@ann @-Ast Type-Expr)
     (List '@if @-Ast @-Ast @-Ast)
     (List '@lit-bytes Bytes)
     (List '@set! Symbol @-Ast)
     (List '@set! Symbol @-Ast)
     (List '@loop Nonnegative-Integer @-Ast)
     (with-context @-Ast)))

(define-type Definition
  (U (List '@def-var Symbol @-Ast)
     (List '@def-fun Symbol
           (Listof (List Symbol Type-Expr))
           (Option Type-Expr)
           @-Ast)))

(define-type Type-Expr
  (U (List '@type-var Symbol)
     (List '@type-vec (Listof Type-Expr))
     (List '@type-vecof Type-Expr Nonnegative-Integer)
     (List '@type-union Type-Expr Type-Expr)
     (List '@type-bytes Nonnegative-Integer)))

(define-type @-Binop (U '@+ '@- '@* '@/ '@append))