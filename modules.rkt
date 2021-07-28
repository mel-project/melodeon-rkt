#lang typed/racket
(require "common.rkt")
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
                  (let ([dep-fname (build-path (path->string
                                                (cast (path-only filename) Path)) dep-fname)])
                    (demodularize (filename->ast dep-fname)
                                  dep-fname
                                  filename->ast))]
                 [_ #f])))
     (Listof @-Ast)))
  ;; Mangle all of our dependencies
  (: mangled (Listof @-Ast))
  (define mangled (for/list ([dep dependencies])
                    (define pfx (gensym 'mangle))
                    (mangle-unprovided
                     dep
                     (λ((x : Symbol))
                       (string->symbol
                        (string-append (symbol->string pfx)
                                       (symbol->string x)))))))
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
      [x x]))
  (parameterize ([current-context (context-of ast)])
    (contextualize
     (match (dectx ast)
       [`(@program ,defs ,inner) `(@program ,(map recurse-def defs)
                                            ,(recurse inner))]
       [`(@let (,k ,v) ,inner)
        `(@let (,(automangle k)
                ,(recurse v))
               ,(mangle-unprovided/inner inner mangle (set-add no-mangle k)))]
       [`(@apply ,f ,args)
        `(@apply ,(automangle f) ,(map recurse args))]
       [`(@var ,sym)
        `(@var ,(automangle sym))]
       ;; all the other stupid cases. This is so stupid and slow it's ridiculous. Will be replaced by a "functor" style impl later
       [(? list? whatever)
        (cast (map (λ(x)
                     (if (@-Ast? x) (recurse x) x))
                   whatever) @-Ast)]
       ))))