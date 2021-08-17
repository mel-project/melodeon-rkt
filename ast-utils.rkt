#lang typed/racket
(require "common.rkt"
         "type-sys/types.rkt")
(require racket/hash)
(require typed-map)
(provide Type-Map
         flatten1
         struct-def?
         ast->list
         ast->list*
         ast-map
         (struct-out return))


; An environment for type variable mappings to types
(define-type Type-Map (Immutable-HashTable Symbol Type))

; flatten 1 level, preserves type info
(: flatten1 (All (T) (-> (Listof (Listof T)) (Listof T))))
(define (flatten1 ls)
  (foldl append '() ls))

(: struct-def? (-> Definition Boolean))
(define (struct-def? def)
  (match def
    [`(@def-struct ,_ ,_) #t]
    [_ #f]))

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

(struct (a) return ((inner : a)) #:transparent
  #:type-name Return)

;; Runs every subexpression of a through f, recursively
(: ast-map (-> @-Ast
               (#:post-recurse (-> @-Ast @-Ast))
               (#:pre-recurse (-> @-Ast (U @-Ast (Return @-Ast))))
               @-Ast))
(define (ast-map a #:post-recurse (f values) #:pre-recurse (g values))
  (define recurse (λ((x : @-Ast)) (ast-map x
                                           #:post-recurse f
                                           #:pre-recurse g)))
  (parameterize ([current-context (context-of a)])
    (contextualize
     (f
      (match (g (dectx a))
        [(Return a) a]
        ; trivials
        [`(@lit-num ,n) `(@lit-num ,n)]
        [`(@lit-bytes ,b) `(@lit-bytes ,b)]
        [`(@var ,x) `(@var ,x)]
        [`(@extern ,s) `(@extern ,s)]

        [`(@let (,var ,val) ,expr) `(@let (,var ,(recurse val))
                                          ,(recurse expr))]
        [`(,(? @-Binop? op) ,a ,b) `(,op ,(recurse a) ,(recurse b))]
        [`(@lit-vec ,v) `(@lit-vec ,(map recurse v))]
        [`(@program ,defs ,expr) `(@program ,(map (λ((def : Definition))
                                                    (match def
                                                      [`(@def-var ,sym ,ast) `(@def-var ,sym ,ast)]
                                                      [`(@def-generic-fun ,name
                                                                          ,type-params
                                                                          ,arguments
                                                                          ,return-type
                                                                          ,body)
                                                       `(@def-generic-fun ,name
                                                                          ,type-params
                                                                          ,arguments
                                                                          ,return-type
                                                                          ,(recurse body))]
                                                      [`(@def-fun ,name
                                                                  ,arguments
                                                                  ,return-type
                                                                  ,body)
                                                       `(@def-fun ,name
                                                                  ,arguments
                                                                  ,return-type
                                                                  ,body)]
                                                      [x x]))
                                                  defs) ,(recurse expr))]
        [`(@apply ,name ,v) `(@apply ,name ,(map recurse v))]
        [`(@block ,v) `(@block ,(map recurse v))]
        [`(@index ,x ,y) `(@index ,(recurse x) ,(recurse y))]
        [`(@update ,e1 ,e2 ,e3) `(@update ,(recurse e1) ,(recurse e2) ,(recurse e3))]
        [`(@unsafe-cast ,e ,t) `(@unsafe-cast ,(recurse e) ,t)]
        [`(@ann ,expr ,t) `(@ann ,(recurse expr) ,t)]
        [`(@if ,p ,tru ,fls) `(@if ,(recurse p) ,(recurse tru) ,(recurse fls))]
        [`(@for ,e1 ,var ,e2) `(@for ,(recurse e1) var ,(recurse e2))]
        ;[`(@set! _ ,expr) (ast-map expr)]
        [`(@loop ,count ,expr) `(@loop ,count ,(recurse expr))]
        [`(@is ,expr ,t) `(@is ,(recurse expr) ,t)]
        [`(@extern-call ,fname ,args) `(@extern-call ,fname ,(map recurse args))]
        [`(@instantiate ,struct-name ,elems) `(@instantiate ,struct-name ,(map recurse elems))]
         [(with-context ctx matter) (with-context ctx (recurse matter))]
         )))))