#lang typed/racket
(require "common.rkt")
(require "type-sys/types.rkt")
;(require "type-sys/typecheck.rkt")

(provide (all-defined-out))

;; Top-level node. This is NOT a $-Ast.
(struct $program
  ([fun-defs : (Listof $fndef)]
   [expr : $-Ast])
  #:transparent)

;; $-Ast represents a desugared, fully typed AST
(struct $-Ast
  ([type : Type]
   [node : $-Ast-variant])
  #:transparent)

(struct $let
  ([bind-var : Symbol]
   [bind-expr : $-Ast]
   [body : $-Ast])
  #:transparent)

; Literal number
(struct $lit-num 
  ([val : Nonnegative-Integer])
  #:transparent)
; Literal vector
(struct $lit-vec
  ([val : (Listof $-Ast)])
  #:transparent)
; Literal bytes
(struct $lit-bytes
  ([val : Bytes])
  #:transparent)
; Literal string
(struct $lit-string
  ([val : String])
  #:transparent)
; Variable name
(struct $var
  ([val : Symbol])
  #:transparent)
(struct $extern
  ([val : String])
  #:transparent)
; Function definition
(struct $fndef
  ([name : Symbol]
   [binds : (Listof (List Symbol Type))]
   [body : $-Ast])
  #:transparent)

(struct $apply
  ([name : Symbol]
   [args : (Listof $-Ast)])
  #:transparent)

(struct $index
  ([data : $-Ast]
   [ref : $-Ast])
  #:transparent)

(struct $loop
  ([count : Nonnegative-Integer]
   [body : $-Ast])
  #:transparent)

(struct $if
  ([pred : $-Ast]
   [true : $-Ast]
   [false : $-Ast])
  #:transparent)

(define-type $-Binop
  (U '+
     '-
     '*
     '/
     'append
     'eq))

; binop
(struct $bin
  ([op : $-Binop]
   [l : $-Ast]
   [r : $-Ast]))

(struct $block
  ([exprs : (Listof $-Ast)])
  #:transparent)


(define-type $-Ast-variant
  (U $let
     ;(List $-Binop $-Ast $-Ast)
     $bin
     $lit-num
     $lit-vec
     $lit-string
     $lit-bytes
     $var
     $extern
     $apply
     $loop
     $block
     $index
     $if))
