#lang typed/racket
(require "raw-ast.rkt"
         "../type-sys/types.rkt"
         compatibility/defmacro)
;(require racket/hash)
(require typed-map)
(provide Type-Map
         flatten1
         struct-def?
         ast->list
         ast->list*
         ast-fold
         ast-fold-def
         zip
         ;def->ast
         ast-map
         @ast-parents
         @def-parents
         (struct-out return))


; An environment for type variable mappings to types
(define-type Type-Map (Immutable-HashTable Symbol Type))

; Zip two lists
(: zip (All (a b) (-> (Listof a) (Listof b) (Listof (Pairof a b)))))
(define (zip l r)
  (for/list ([x l] [y r])
    (cons x y)))

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
                                                                          ,const-params
                                                                          ,arguments
                                                                          ,return-type
                                                                          ,body)
                                                       `(@def-generic-fun ,name
                                                                          ,type-params
                                                                          ,const-params
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

; Fold over a definition, calling ast-fold  on inner ast nodes
(: ast-fold-def (All (A) (-> (-> @-Ast A A) Definition A A)))
(define (ast-fold-def f def initial)
   (match def
     [`(@def-generic-fun ,_ ,_ ,_ ,_ ,_ ,body)
       (ast-fold f body initial)]
     [`(@def-struct ,_ ,fields)
       ; TODO
       ;(foldl (texpr-fold f body initial) (map cadr fields))]))))
       initial]
       #|
     [`(@def-alias ,n ,texpr)
       |#
     [`(@def-generic-fun ,_ ,_ ,_ ,_ ,_ ,body)
       (ast-fold f body initial)]
     [`(@def-var ,_ ,body)
       (ast-fold f body initial)]
     [`(@def-fun ,_ ,_ ,_ ,body)
       (ast-fold f body initial)]
     [_ initial]))

; Fold over an @-Ast, recursing on inner nodes
(: ast-fold (All (A) (-> (-> @-Ast A A) @-Ast A A)))
(define (ast-fold f ast acc)
  (: initial A)
  (define initial (f (dectx ast) acc))

   (match (dectx ast)
     [`(@program ,defs ,body)
       (ast-fold f body (foldl (λ(def acc) (ast-fold-def f def acc)) initial defs))]
     [`(@lit-num ,n) initial]
     [`(@var ,x) initial]
     [`(@let (,var ,val) ,expr)
       (ast-fold f expr
                (ast-fold f val initial))]
     [`(@apply ,_ ,asts)
       (foldl (λ(ast v) (ast-fold f ast v)) initial asts)]
     [_ initial]))

; Wraps a definition in a root-level
; @program ast node.
#|
(: def->ast (-> Definition @-Ast))
(define (def->ast def)
  `(@program ,(list def) (@empty)))
|#

(: memoize-thunk (All (a) (-> (-> a) (-> a))))
(define (memoize-thunk th)
  (: memory (Listof a))
  (define memory '())
  (lambda ()
    (if (empty? memory)
        (let ([res (th)])
          (set! memory (list res))
          res)
        (car memory))))



;; A general-purpose ast transformer. Takes in a function with three arguments: an ast node, a function that, given a 1-index number n, returns the nth subexpression processed through the same function, and an integer which counts the number of subexpressions.
;; The ast-recombine function may be useful when transforming an ast into an ast.
(: @ast-recurse (All (a) (-> (-> @-Ast (-> Integer a) Nonnegative-Integer a) @-Ast a)))
(define (@ast-recurse fun ast)
  (: devec (All (b) (-> (Vectorof (-> b)) (-> Integer b))))
  (define (devec vec)
    (λ(n) ((vector-ref vec n))))
  (: recurse (-> @-Ast a))
  (define (recurse ast)
    (@ast-recurse fun ast))

  (define-macro (% . rst)
    `(devec (vector . ,(map (λ(elem) `(memoize-thunk (λ() (recurse ,elem)))) rst))))
  ; Same thing a % but on a list
  (define-macro (%-vec v)
    `(devec (list->vector
      (map (λ(elem) (memoize-thunk (λ() (recurse elem))))
           ,v))))

  (match ast
    [(with-context ctx inner) (fun ast (% inner) 1)]
    ; trivials
    [`(@lit-num ,_) (fun ast (%) 0)]
    [`(@lit-bytes ,_) (fun ast (%) 0)]
    [`(@var ,_) (fun ast (%) 0)]
    [`(@extern ,_) (fun ast (%) 0)]
    ;[`(@empty) (fun ast (%) 0)]
    ; others
    [`(@let (,var ,val) ,expr) (fun ast (% val expr) 2)]
    [`(,(? @-Binop? op) ,a ,b) (fun ast (% a b) 2)]
    [`(@unsafe-cast ,e ,_) (fun ast (% e) 1)]
    [`(@index ,e1 ,e2) (fun ast (% e1 e2) 2)]
    [`(@init-vec ,e ,_) (fun ast (% e) 1)]
    [`(@program ,_ ,e) (fun ast (% e) 1)]
    [`(@accessor ,e ,_) (fun ast (% e) 1)]
    [`(@ann ,e ,_) (fun ast (% e) 1)]
    [`(@is ,e ,_) (fun ast (% e) 1)]
    [`(@loop ,_ ,e) (fun ast (% e) 1)]
    [`(@range ,a ,b) (fun ast (% a b) 2)]
    [`(@slice ,a ,b ,c) (fun ast (% a b c) 3)]
    [`(@update ,a ,b ,c) (fun ast (% a b c) 3)]
    [`(@if ,a ,b ,c) (fun ast (% a b c) 3)]
    [`(@for ,a ,_ ,b) (fun ast (% a b) 2)]
    [`(@fold ,expr ,_ ,_ ,_ ,l)
      (fun ast (% expr l) 2)]
    [`(@block ,v)
      (fun ast (%-vec v) (length v))]
    [`(@lit-vec ,v)
      (fun ast (%-vec v) (length v))]
    [`(@instantiate ,_ ,v)
      (fun ast (%-vec v) (length v))]
    [`(@apply ,_ ,v)
     (fun ast (%-vec v) (length v))]
    [`(@extern-call ,_ ,v)
     (fun ast (%-vec v) (length v))]
    ))

(: @def-parents (-> Definition (Setof Symbol)))
(define (@def-parents def)
  (match def
    ; TODO check type expr for parents
    [`(@def-fun ,n ,params ,_ ,body)
     (set-union (for/fold ([accum : (Setof Symbol) (set)])
                          ([pair : (List Symbol Type-Expr) params])
                  (set-union accum (type-expr-parents (second pair))))
                (foldl (λ(param acc) (set-remove acc param))
                       (@ast-parents body)
                       (map car params)))]
    [`(@def-generic-fun ,n ,tvars ,cvars ,params ,_ ,body)
     (set-union (for/fold ([accum : (Setof Symbol) (set)])
                          ([pair : (List Symbol Type-Expr) params])
                  (set-union accum (type-expr-parents (second pair))))
                (foldl (λ(param acc) (set-remove acc param))
                       (@ast-parents body)
                       (append cvars tvars (map car params))))]
    [_ (set)]))

(: @ast-parents (-> @-Ast (Setof Symbol)))
(define (@ast-parents ast)
  ((inst @ast-recurse (Setof Symbol))
   (λ (ast $ N)
     (match ast
       [`(@var ,(? symbol? x)) (set x)]
       [`(@apply ,(? symbol? fun-name) ,body) (set-union (set fun-name)
                                                         (for/fold ([accum : (Setof Symbol) (set)])
                                                                   ([counter (length body)])
                                                           (set-union accum ($ counter))))]
       [`(@let (,var ,_) ,_) (set-remove (set-union ($ 0)
                                                    ($ 1))
                                         var)]
       [_ (for/fold ([accum : (Setof Symbol) (set)])
                    ([counter N])
            (set-union accum ($ counter)))]))
   ast))

(: type-expr-parents (-> Type-Expr (Setof Symbol)))
(define (type-expr-parents texpr)
  (match texpr
    [`(@type-var ,sym) (set sym)]
    [`(@type-struct ,_ ,pairs) (for/fold ([accum : (Setof Symbol) (set)])
                                         ([pair pairs])
                                 (set-union (type-expr-parents (cast (second pair) Type-Expr)) accum))]
    [`(@type-vec ,lst) (for/fold ([accum : (Setof Symbol) (set)])
                                 ([elem lst])
                         (set-union (type-expr-parents elem) accum))]
    [`(@type-vecof ,expr ,_) (type-expr-parents expr)]
    [`(@type-dynvecof ,expr) (type-expr-parents expr)]
    [`(@type-union ,t ,u) (set-union (type-expr-parents t)
                                     (type-expr-parents u))]
    [`(@type-intersect ,t ,u) (set-union (type-expr-parents t)
                                         (type-expr-parents u))]
    [_ (set)]))


;; Demo of a "scopeful" use-case: the same function, implemented with a "blacklist" strategy
#|
(: @demo (-> @-Ast (Setof Symbol) (Setof Symbol)))
(define (@demo ast blacklist)
  ((inst @ast-recurse (Setof Symbol))
   (λ (ast $ N)
     (match ast
       [`(@var ,(? symbol? x)) (set x)]
       [`(@let (,var ,var-bind) ,inner)
        ;; Manually recurse *for this one case*
        (set-union (@demo var-bind blacklist)
                   (@demo inner (set-add blacklist var)))
        ]
       [_ (for/fold ([accum : (Setof Symbol) (set)])
                    ([counter N])
            (set-union accum ($ counter)))]))
   ast))
|#

;(@ast-unshadowed-symbols '(@let (x (@let (y (@lit-num 2)) (@var y))) (@var x)))
;(@demo '(@let (x (@let (y (@lit-num 2)) (@var y))) (@var y))
;       (set))