#lang typed/racket

(require "common.rkt"
         "typed-ast.rkt"
         "type-sys/typecheck.rkt"
         "type-sys/types.rkt")

(provide @-Ast->$-Ast)

;; mangle a Melodeon symbol to a Mil symbol
(: mangle-sym (-> Symbol Symbol))
(define (mangle-sym melo-sym)
  ; do nothing atm
  melo-sym)

(: new-$-Ast (-> @-Ast Type-Scope $-Ast-variant $-Ast))
(define (new-$-Ast ast env val)
  ($-Ast
    (@-ast->type+ ast env)
    env
    val))

(: fun-def->$fndef (-> Definition Type-Scope $fndef))
(define (fun-def->$fndef def env)
  (match def
    [`(@def-fun ,var ,args ,_ ,expr)
      ($fndef
        var
        (map (lambda ((x : (List Symbol Type-Expr)))
               (list
                 (car x)
                 (resolve-type-or-err
                   (cadr x)
                   (Type-Scope-type-vars env))))
              args)
        (@-Ast->$-Ast expr env))]))

(: filter-fn-defs (-> (Listof Definition) (Listof Definition)))
(define (filter-fn-defs defs)
  (filter (lambda ((def : Definition))
            (match def
              [`(@def-fun _ _ _ _) #t]
              [_ #f]))
          defs))

(: @program->$program (-> @-Ast (List $program Type-Scope)))
(define (@program->$program @ast)
  (match @ast
    [`(@program ,definitions ,body)
      ; Add definitions to the env
      (define env (definitions->scope definitions))
      ; Build $-Ast as fun defs followed by expression
      (list
        ($program
          (map (lambda ((def : Definition))
                 (fun-def->$fndef def env))
               (filter-fn-defs definitions))
          (@-Ast->$-Ast body env))
        env)]
    [_ (error "expected a @program ast node")]))

(: @-Ast->$-Ast (-> @-Ast Type-Scope $-Ast))
(define (@-Ast->$-Ast @-ast env)
  ;(: strip-@ (-> Symbol Symbol))
  ;(define (strip-@ @-sym)
  ;  (string->symbol (substring (symbol->string @-sym) 1)))

  (let ([raw-ast (dectx @-ast)])
  (match raw-ast;(dectx @-ast)
    ;; casts etc
    [`(@unsafe-cast ,inner ,_) (@-Ast->$-Ast inner env)]
    [`(@extern ,str)
      (new-$-Ast
        raw-ast
        env
        ($lit-string str))]

                       ;(foldl add-fun-def empty-ts definitions))
    ;; let bindings
    [`(@let (,var-name ,var-value) ,body)
      (new-$-Ast raw-ast env
        ($let (list (mangle-sym var-name)
                    (@-Ast->$-Ast var-value env))
              (@-Ast->$-Ast body env)))]

    [`(@and ,x ,y)
      (let ([$x (@-Ast->$-Ast x env)]
            [$y (@-Ast->$-Ast y env)])
        (new-$-Ast raw-ast env
          ($and $x $y)))]
    [`(@or ,x ,y)
      (let ([$x (@-Ast->$-Ast x env)]
            [$y (@-Ast->$-Ast y env)])
        (new-$-Ast raw-ast env
          ($or $x $y)))]
    [`(@var ,(? symbol? varname))
      (new-$-Ast raw-ast env
        ($var (mangle-sym varname)))]
    #|
    ;; binary ops
    [`(,(? (lambda(op) (member op '(@+ @- @* @/))) op) ,x ,y) `(,(strip-@ op) ,(@-Ast->$-Ast x)
                                                                              ,(@-Ast->$-Ast y))]
    [`(@eq ,x ,y) ; equality generation is special because it's type-dependent
     (generate-eq-mil x y)]
    [`(@var ,(? symbol? varname)) (mangle-sym varname)]
    [`(@lit-num ,(? exact-integer? number)) number]
    [`(@lit-vec ,vv) `(vector . ,(map @-Ast->$-Ast vv))]

    ;; other stuff
    [`(@apply ,fun ,args) `(,(mangle-sym fun) . ,(map @-Ast->$-Ast args))]
    [`(@append ,x ,y) `(,(match (memoized-type x)
                           [(TVectorof _ _) 'v-concat]
                           [(TVector _) 'v-concat]
                           [(TBytes _) 'b-concat])
                        ,(@-Ast->$-Ast x)
                        ,(@-Ast->$-Ast y))]
    [`(@index ,vec ,idx) `(,(match (memoized-type vec)
                              [(TVectorof _ _) 'v-get]
                              [(TVector _) 'v-get]
                              [(TBytes _) 'b-concat]) ,(@-Ast->$-Ast vec)
                                                      ,(@-Ast->$-Ast idx))]
    [`(@if ,x ,y ,z) `(if ,(@-Ast->$-Ast x)
                          ,(@-Ast->$-Ast y)
                          ,(@-Ast->$-Ast z))]
    [`(@for ,expr ,var-name ,vec-val)
     (let ([count (match (memoized-type vec-val)
                    [(TVectorof _ count) count]
                    [(TVector v) (length v)]
                    [(TBytes b) b])]
           [counter (gensym 'fori)]
           [tempvec (gensym 'forv)])
       `(let (,counter 0 ,tempvec ,(@-Ast->$-Ast vec-val))
          (loop ,count (set-let ()
                                (set! ,tempvec (v-from ,tempvec ,counter
                                                       (let (,var-name (v-get ,tempvec iter))
                                                         ,(@-Ast->$-Ast expr))))
                                (set! ,counter (+ ,counter 1))))
          ,tempvec)
       )]
    [`(@is ,expr ,type)
     ; TODO: somehow integrate with type resolving

     ; *********************
     ; THIS DOESN'T WORK (empty scope) AND IS TEMPORARY TO GET CODE TO BUILD
     ; *********************
     (define resolved-type (resolve-type-or-err type (make-immutable-hash)))
     (define tmpsym (gensym 'is))
     `(let (,tmpsym ,(@-Ast->$-Ast expr))
        ,(generate-is resolved-type tmpsym))]
    [`(@lit-bytes ,bts) (string->symbol
                         (string-append "0x"
                                        (bytes->hex-string bts)))]
    [`(@ann ,inner ,_) (@-Ast->$-Ast inner)]
    [`(@block ,inner) `(let () . ,(map @-Ast->$-Ast inner))]
    [`(@set! ,x ,y) `(set! ,x ,(@-Ast->$-Ast y))]
    [`(@loop ,n ,body) `(loop ,n ,(@-Ast->$-Ast body))]
    |#
    [other (error "invalid @-ast" other)])))

#|
(: def->$-Ast (-> Definition $-Ast))
(define (generate-mil-defs def)
  (match def
    [`(@def-var ,var ,expr) `(gl ,(mangle-sym var) ,(generate-mil expr))]
    [`(@def-fun ,var ,args ,_ ,expr)
     `(fn ,(mangle-sym var) ,(map mangle-sym (map (inst car Symbol Any) args)) ,(generate-mil expr))]))
|#
