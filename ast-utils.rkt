#lang typed/racket
(require "common.rkt"
         "type-sys/types.rkt")
(require racket/hash)
(provide Type-Map
         ast->list
         ast->list*)


; An environment for type variable mappings to types
(define-type Type-Map (Immutable-HashTable Symbol Type))

(: ast->list* (-> (Listof @-Ast) (Listof @-Ast)))
(define (ast->list* v)
  (append* (map ast->list v)))

; Recursively serialize an ast into a list.
; Useful as a precursor to fold and map which expect a list.
(: ast->list (-> @-Ast (Listof @-Ast)))
(define (ast->list a)
  (cons a (match a
    ; TODO map over binding expressions of let
    [`(@lit-vec ,v) (ast->list* v)]
    [`(@let _ ,expr) (ast->list expr)]
    [`(@-Binop ,a ,b) (append (ast->list a) (ast->list b))]
    [`(@lit-num _) (list)]
    [`(@lit-vec ,v) (ast->list* v)]
    [`(@var _) (list)]
    [`(@program _ ,expr) (ast->list expr)]
    [`(@apply _ ,v) (ast->list* v)]
    [`(@block ,v) (ast->list* v)]
    [`(@index ,x ,y) (append (ast->list x) (ast->list y))]
    [`(@update ,e1 ,e2 ,e3) (append (ast->list e1) (ast->list e2) (ast->list e3))]
    [`(@unsafe-cast ,e _) (ast->list e)]
    [`(@ann ,expr _) (ast->list expr)]
    [`(@if ,p ,t ,f) (append (ast->list p) (ast->list t) (ast->list f))]
    [`(@for ,e1 _ ,e2) (append (ast->list e1) (ast->list e2))]
    [`(@list-bytes _) (list)]
    [`(@set! _ ,expr) (ast->list expr)]
    [`(@loop _ ,expr) (ast->list expr)]
    [`(@extern _) (list)]
    [`(@is ,expr _) (ast->list expr)]
    [(with-context _ matter) (ast->list matter)]
    )))

(: ast-map (-> (-> @-Ast @-Ast) @-Ast @-Ast))
(define (ast-map f a)
  (match a
    ; trivials
    [`(@lit-num ,n) `(@lit-num ,n)]
    [`(@list-bytes ,b) `(@list-byes ,b)]
    [`(@var ,x) `(@var ,x)]
    [`(@extern ,s) `(@extern ,s)]

    ; TODO map over binding expressions of let?
    [`(@let ,binds ,expr) `(@let ,binds ,(f expr))]
    [`(@-Binop ,a ,b) `(@-Binop ,(f a) ,(f b))]
    [`(@lit-vec ,v) `(@lit-vec ,(map f v))]
    [`(@program ,defs ,expr) `(@program ,defs ,(f expr))]
    [`(@apply ,name ,v) `(@apply ,name ,(map f v))]
    [`(@block ,v) `(@block ,(map f v))]
    [`(@index ,x ,y) `(@index ,(f x) ,(f y))]
    [`(@update ,e1 ,e2 ,e3) `(@update ,(f e1) ,(f e2) ,(f e3))]
    [`(@unsafe-cast ,e ,t) `(@unsafe-cast ,(f e) ,t)]
    [`(@ann ,expr ,t) `(@ann ,(f expr) ,t)]
    [`(@if ,p ,tru ,fls) `(@if ,(f p) ,(f tru) ,(f fls))]
    [`(@for ,e1 ,var ,e2) `(@for ,(f e1) var ,(f e2))]
    ;[`(@set! _ ,expr) (ast-map expr)]
    [`(@loop ,count ,expr) `(@loop ,count ,(f expr))]
    [`(@is ,expr ,t) `(@is ,(f expr) ,t)]
    [(with-context ctx matter) (with-context ctx (ast-map f matter))]
    ))
