#lang typed/racket
(require typed-map)
(require "./common.rkt")
(require "./type-sys/typecheck.rkt")

(: zip (All (a b) (-> (Listof a) (Listof b) (Listof (List a b)))))
(define (zip l r)
  (for/list ([x l] [y r])
    (list x y)))

(: get-generic-fns (-> (Listof Definition) (Listof Definition)))
(define (get-generic-fns defs)
  (filter (lambda ([def : Definition]) (eq? (car def) '@def-generic-fun)) defs))

(: const-generic? (-> Definition Boolean))
(define (const-generic? def)
  (match def
    [`(@def-generic-fun ,_ ,_ ,const-vars ,_ ,_ ,_)
      (> 0 (length const-vars))]
    [_ #f]))

; Get all @apply's in an @-Ast node which fully specify
; constant generic parameters with integer values.
(: get-full-const-apps (-> @-Ast (Listof @-Ast)))
;(define (get-full-const-apps ast)

; Returns true if the constant expressions are all integers
; and match the number of const vars to the given @apply node.
(: is-full-const-app (-> (Listof Const-Exprs) @-Ast Boolean))
(define (is-full-const-app args app)
  (match (dectx app)
    [`(@apply ,_ ,args)
      (map (λ(arg) (@->$ arg ts) args)

; Get all function definitions which have constant generic parameters
(: get-const-generic-fns (-> (Listof Definition) (Listof Definition)))
(define (get-const-generic-fns defs)
  (filter const-generic? defs))

#|
(: sym->type-var (-> Symbol Type-Expr))
(define (sym->type-var s) `(@type-var ,s))
|#

; Check if a type contains another type
; TODO right now we don't have alias/structs so type-var simply checks equality
#|
(: type-contains (-> Type-Expr Type-Expr Boolean))
(define (type-contains typ inner-typ)
  (match typ
    [`(@type-var ,t) #:when (eq? t inner-typ) #t]
    [`(@type-vec ,v)
      ; check that inner-typ exists at least once in type vector
      (foldl (lambda ([x : Type-Expr] [acc : Boolean])
               (or acc (type-contains x inner-typ)))
             #f
             v)]
    [`(@type-vecof ,t) (eq? t inner-typ)]
    [`(@type-union ,x ,y)
      (or (type-contains x inner-typ)
          (type-contains y inner-typ))]
    [`(@type-bytes _) #f]))
|#

; Takes a @def-generic-fun definition and an @application ast node
; and returns a @def-fun definition where the generic types are replaced
; with types inferred by the application; or nothing if inference is not
; possible.
#|
(: generic->concrete (-> Definition @-Ast Definition))
(define (generic->concrete gen-def app)
  (cond
    [(match gen-def [`(@def-generic-fun _ _ _ _ _) #t]
                    [_ #f])
     #f]
    [(match app [`(@apply _ _) #t]
                [_ #f])
     #f]
    [else
      (letrec
        ([generic-syms : (Listof Symbol)
           (match gen-def
             [`(@def-generic-fun _ ,syms _ _ _) syms]
             [_ (error "BUG: ast should already be checked to be
                       @def-generic-fun") (list)])]
         [param-types : (Listof Type-Expr)
           (match gen-def
             [`(@def-generic-fun _ _ ,binds _ _)
                 (map (lambda ([bind : (List Symbol Type-Expr)]) (cadr bind)) )]
             [_ (error "BUG: ast should already be checked to be
                       @def-generic-fun") (list)])]
         [arg-types : (Listof Type)
           (match app
             [`(@apply _ ,args)
               (map (lambda ([a : @-Ast]) (@-ast->type a)) args)]
             [_ (error "BUG: ast should already be checked to be @apply") (list)])]
         ; Indices of parameters containing generic types.
         [param-idxs
           (flatten (map
             (lambda ([generic-sym : Symbol])
               (filter (lambda ([pair : (List Index Type-Expr)])
                         (let ([i (car pair)] [t (cadr pair)])
                           (type-contains t (sym->type-var generic-sym))))
                       (zip (range (length param-types)) param-types)))
             generic-syms))])
        ; Produce a concrete fn definition from argument types
        (match gen-def
          [`(@def-generic-fun ,name _ ,binds ,return-type ,body)
            (let
              [concrete-binds
                (map (lambda ([enumerated-bind : (List Index (List Symbol Type-Expr))])
                       (let [i (car enumerated-bind)]
                         ; Return a new binding with same name
                         (list (cdar enumerated-bind)
                               ; if this parameter is generic, replace with the
                               ; argument type, otherwise use parameter
                               ; definition type.
                               (if (member i param-idxs)
                                   (list-ref arg-types i)
                                   (cddar enumerated-bind)))))
                     (zip (range (length binds)) binds))]
              ; TODO implement
              [concrete-return-type #f]
              ; TODO check the body for applications of @def-generic-fun,
              ; recursively resolve
              [concrete-body body]
            `(@def-fun
               ,name
               ,concrete-binds
               ,concrete-return-type,
               ,concrete-body)])))]))
|#
