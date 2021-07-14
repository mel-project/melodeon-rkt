#lang typed/racket
(require "./common.rkt")
(require "./type-sys/typecheck.rkt")

(: zip (All (a b) (-> (Listof a) (Listof b) (Listof (List a b)))))
(define (zip l r)
  (for/list ([x l] [y r])
    (list x y)))

(: get-generic-fns (-> (Listof Definition) (Listof Definition)))
(define (get-generic-fns defs)
  (filter (lambda ([def : Definition]) (eq? (car def) '@def-generic-fun)) defs))

(: sym->type-var (-> Symbol Type-Expr))
(define (sym->type-var s) `(@type-var ,s))

; Check if a type contains another type
; TODO right now we don't have alias/structs so type-var simply checks equality
(: type-contains (-> Type-Expr Type-Expr Boolean))
(define (type-contains typ inner-typ)
  (match typ
    [`(@type-var ,t) #:when (eq? t inner-typ) #t]
    [`(@type-vec ,v)
      ; check that inner-typ exists at least once in type vector
      (foldl (lambda ([acc : Boolean] [x : Type-Expr])
               (or acc (type-contains x inner-typ)))
             #f
             v)]
    [`(@type-vecof ,t) (eq? t inner-typ)]
    [`(@type-union ,x ,y)
      (or (type-contains x inner-typ)
          (type-contains y inner-typ))]
    [`(@type-bytes _) #f]))

; Takes a @def-generic-fun definition and an @application ast node
; and returns a @def-fun definition where the generic types are replaced
; with types inferred by the application; or nothing if inference is not
; possible.
(: generic->concrete (-> Definition @-Ast (Option Definition)))
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
                       (zip (range (length arg-types)) arg-types)))
             generic-syms))])
        #f)]))
