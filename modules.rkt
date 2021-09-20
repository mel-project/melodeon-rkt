#lang typed/racket
(require "asts/raw-ast.rkt"
         "asts/ast-utils.rkt")
(provide demodularize)

;; Fully "demodularizes" an @-Ast, given its filename and a file-loading function.
(: demodularize
   (-> @-Ast
       Path-String
       (-> Path-String @-Ast)
       @-Ast))
(define (demodularize ast
                      filename
                      filename->ast)
  (match-define `(@program ,definitions ,inner) (dectx ast))
  ;; Gather demodularized dependencies recursively
  (: dependencies (Listof @-Ast))
  (define dependencies
    (cast 
     (filter (λ((x : (Option @-Ast))) x)
             (for/list ([definition (in-list definitions)]) : (Listof (Option @-Ast))
               (match definition
                 [`(@require ,dep-fname)
                  (let ([dep-fname (if (absolute-path? dep-fname) dep-fname
                                       (build-path (path->string
                                                    (cast (path-only filename) Path)) dep-fname))])
                    (demodularize (filename->ast dep-fname)
                                  dep-fname
                                  filename->ast))]
                 [_ #f])))
     (Listof @-Ast)))
  ;; Mangle all of our dependencies
  (: mangled (Listof @-Ast))
  (define mangled (for/list ([dep dependencies])
                    (define pfx (gensym 'M))
                    (mangle-unprovided
                     dep
                     (λ((x : Symbol))
                       (if (string-contains? (symbol->string x)
                                             (symbol->string pfx))
                           x
                           (string->symbol
                            (string-append (symbol->string pfx)
                                           "__"
                                           (symbol->string x))))))))
  ;; "Inject" the dependencies
  (parameterize ([current-context (context-of ast)])
    (contextualize
     `(@program ,(append (append* (map (λ((x : @-Ast))
                                         (match (dectx x)
                                           [`(@program ,defs ,_) defs]))
                                       mangled))
                         definitions)
                ,inner))))

;; Mangles the unprovided names of a fully demodularized @-Ast, given a function that maps a symbol to a symbol
(: mangle-unprovided (-> @-Ast (-> Symbol Symbol) @-Ast))
(define (mangle-unprovided ast mangle)
  (match-define `(@program ,definitions ,inner) (dectx ast))
  ;; Gather the non-provided global definitions
  (define provided (list->set (for/list ([def definitions]) : (Listof Symbol)
                                (match def
                                  [`(@provide ,sym) sym]
                                  [else '__dummy]))))
  ;(pretty-print provided)
  (mangle-unprovided/inner ast mangle provided))

;; Inner function that recursively keeps track of a list of not-to-mangle symbol names
(: mangle-unprovided/inner (-> @-Ast (-> Symbol Symbol)
                               (Setof Symbol)
                               @-Ast))
(define (mangle-unprovided/inner ast mangle no-mangle)
  (define automangle (λ((x : Symbol)) (if (set-member? no-mangle x) x (mangle x))))
  (define recurse (λ((x : @-Ast)) (mangle-unprovided/inner x mangle no-mangle)))
  (: recurse-def (-> Definition Definition))
  (define (recurse-def def)
    (match def
      [`(@def-var ,sym ,ast) `(@def-var ,(automangle sym)
                                        ,(recurse ast))]
      [`(@def-fun ,sym ,args ,restype ,ast) `(@def-fun ,(automangle sym)
                                                       ,args
                                                       ,restype
                                                       ,(mangle-unprovided/inner
                                                         ast mangle
                                                         (set-union no-mangle
                                                                    (list->set (map (inst car Symbol) args)))))]
      [`(@def-struct ,sym ,lalas)
       `(@def-struct ,(automangle sym)
                     ,(for/list ([elem lalas]) : (Listof (List Symbol Type-Expr))
                        (match elem
                          [(list sym texpr)
                           (list (cast sym Symbol)
                                 (mangle-type-expr (cast texpr Type-Expr)
                                                   (λ((x : Symbol))
                                                     (if (set-member? no-mangle x) x (mangle x)))))])))]
      [`(@def-alias ,t ,u) `(@def-alias ,(automangle t) ,(mangle-type-expr (cast u Type-Expr)
                                                                           (λ((x : Symbol))
                                                                             (if (set-member? no-mangle x) x (mangle x)))))]
      [x x]))
  (parameterize ([current-context (context-of ast)])
    (contextualize
     (match (dectx ast)
       [`(@program ,defs ,inner) `(@program ,(map recurse-def defs)
                                            ,(recurse inner))]
       [node (ast-map #:pre-recurse
                      (lambda ((node : @-Ast))
                        (match node
                          [`(@let (,var ,val) ,expr) (return
                                                      `(@let (,var ,(recurse val))
                                                             ,(mangle-unprovided/inner expr
                                                                                       mangle
                                                                                       (set-add no-mangle var))))]
                          [x x]))
                      #:post-recurse
                      (lambda ((node : @-Ast))
                        (match node
                          [`(@var ,x) `(@var ,(automangle x))]
                          [`(@apply ,f ,args) `(@apply ,(automangle f) ,args)]
                          [`(@unsafe-cast ,x ,t) `(@unsafe-cast ,x ,(mangle-type-expr t
                                                                                      automangle))]
                          [x x]))
                      node)]))))

(: mangle-type-expr (-> Type-Expr (-> Symbol Symbol) Type-Expr))
(define (mangle-type-expr expr mangle)
  (: recurse (-> Type-Expr Type-Expr))
  (define (recurse expr)
    (mangle-type-expr expr mangle))
  (match expr
    [`(@type-var Nat) expr]
    [`(@type-var Any) expr]
    [`(@type-var ,sym) `(@type-var ,(mangle sym))]
    [`(@type-vec ,inner) `(@type-vec ,(map recurse inner))]
    [`(@type-vecof ,t ,n) `(@type-vecof ,(recurse t) ,n)]
    [`(@type-dynvecof ,t) `(@type-dynvecof ,(recurse t))]
    [`(@type-union ,t ,u) `(@type-union ,(recurse t)
                                        ,(recurse u))]
    [`(@type-intersect ,t ,u) `(@type-intersect ,(recurse t)
                                                ,(recurse u))]
    [x x]))