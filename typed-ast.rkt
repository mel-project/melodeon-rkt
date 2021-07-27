#lang typed/racket
(require "common.rkt")
(require "type-sys/types.rkt")
(require "type-sys/typecheck.rkt")

(provide (all-defined-out))

(struct $-Ast
  ([type : Type]
   [scope : Type-Scope]
   [node : $-Ast-variant]))

(struct $let
  ([binds : (List Symbol $-Ast)]
   [body : $-Ast]))

; Literal number
(struct $lit-num
  ([val : Nonnegative-Integer]))
; Literal vector
(struct $lit-vec
  ([val : (Listof $-Ast)]))
; Literal bytes
(struct $lit-bytes
  ([val : Bytes]))
; Literal string
(struct $lit-string
  ([val : String]))
; Variable name
(struct $var
  ([val : Symbol]))
; Function definition
(struct $fndef
  ([name : Symbol]
   [binds : (Listof (List Symbol Type))]
   [body : $-Ast]))
; Top-level node
(struct $program
  ([fun-defs : (Listof $fndef)]
   [expr : $-Ast]))

(struct $apply
  ([name : Symbol]
   [args : (Listof $-Ast)]))

(struct $index
  ([data : $-Ast]
   [ref : $-Ast]))

(struct $loop
  ([count : Nonnegative-Integer]
   [body : $-Ast]))

(struct $if
  ([pred : $-Ast]
   [true : $-Ast]
   [false : $-Ast]))

; binop
(struct $and
  ([l : $-Ast]
   [r : $-Ast]))
(struct $or
  ([l : $-Ast]
   [r : $-Ast]))
(struct $+
  ([l : $-Ast]
   [r : $-Ast]))
(struct $-
  ([l : $-Ast]
   [r : $-Ast]))
(struct $*
  ([l : $-Ast]
   [r : $-Ast]))
(struct $/
  ([l : $-Ast]
   [r : $-Ast]))
(struct $append
  ([l : $-Ast]
   [r : $-Ast]))

(struct $eq
  ([l : $-Ast]
   [r : $-Ast]))

;(define-type $-Binop (U '$+ '$- '$* '$/ '$append '$or '$and '$eq))

(struct $block
  ([exprs : (Listof $-Ast)]))

(define-type $-Ast-variant
  (U $let
     ;(List $-Binop $-Ast $-Ast)
     $and
     $or
     $+
     $-
     $*
     $/
     $append
     $eq
     $lit-num
     $lit-vec
     $lit-string
     $lit-bytes
     $var
     $apply
     $loop
     $block
     $index
     $if))
