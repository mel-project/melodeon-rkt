#lang typed/racket

(require "common.rkt"
         "typed-ast.rkt"
         "type-sys/typecheck.rkt"
         "type-sys/types.rkt")

(provide @-Ast->$-Ast
         @program->$program)

;; mangle a Melodeon symbol to a Mil symbol
(: mangle-sym (-> Symbol Symbol))
(define (mangle-sym melo-sym)
  ; do nothing atm
  melo-sym)

;(: new-$-Ast (-> @-Ast Type-Scope $-Ast-variant $-Ast))
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
  (match (dectx @ast)
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
    [other (error "expected a @program ast node, got ~a" other)]))

(: @-Ast->$-Ast (-> @-Ast Type-Scope $-Ast))
(define (@-Ast->$-Ast @-ast env)
  ;(: strip-@ (-> Symbol Symbol))
  ;(define (strip-@ @-sym)
  ;  (string->symbol (substring (symbol->string @-sym) 1)))

  ;(printf "env: ~a\n" env)
  ;(printf "ast ~a\n" @-ast)
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
      (let ([$v (@-Ast->$-Ast var-value env)])
        (new-$-Ast raw-ast env
          ($let (list (mangle-sym var-name) $v)
                (@-Ast->$-Ast body (bind-var env var-name ($-Ast-type $v))))))]

    ; TODO check equality of types on l and r
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
    [`(@+ ,x ,y)
      (let ([$x (@-Ast->$-Ast x env)]
            [$y (@-Ast->$-Ast y env)])
        (new-$-Ast raw-ast env
          ($+ $x $y)))]
    [`(@- ,x ,y)
      (let ([$x (@-Ast->$-Ast x env)]
            [$y (@-Ast->$-Ast y env)])
        (new-$-Ast raw-ast env
          ($- $x $y)))]
    [`(@* ,x ,y)
      (let ([$x (@-Ast->$-Ast x env)]
            [$y (@-Ast->$-Ast y env)])
        (new-$-Ast raw-ast env
          ($* $x $y)))]
    [`(@/ ,x ,y)
      (let ([$x (@-Ast->$-Ast x env)]
            [$y (@-Ast->$-Ast y env)])
        (new-$-Ast raw-ast env
          ($/ $x $y)))]
    [`(@append ,x ,y)
      (let ([$x (@-Ast->$-Ast x env)]
            [$y (@-Ast->$-Ast y env)])
        (new-$-Ast raw-ast env
          ($append $x $y)))]
    [`(@index ,x ,y)
      (let ([$x (@-Ast->$-Ast x env)]
            [$y (@-Ast->$-Ast y env)])
        (new-$-Ast raw-ast env
          ($index $x $y)))]
    [`(@eq ,x ,y)
      (let ([$x (@-Ast->$-Ast x env)]
            [$y (@-Ast->$-Ast y env)])
        (new-$-Ast raw-ast env
          ($eq $x $y)))]

    [`(@var ,(? symbol? varname))
      (new-$-Ast raw-ast env
        ($var (mangle-sym varname)))]
    [`(@lit-num ,(? exact-integer? number))
      (new-$-Ast raw-ast env ($lit-num number))]
    [`(@lit-vec ,vv)
      (new-$-Ast raw-ast env
        ($lit-vec (map (lambda ((x : @-Ast)) (@-Ast->$-Ast x env)) vv)))]
    ;; other stuff
    [`(@apply ,fun ,args)
      (new-$-Ast raw-ast env
        ($apply fun (map (lambda ((x : @-Ast)) (@-Ast->$-Ast x env)) args)))]
    [`(@if ,x ,y ,z)
      (new-$-Ast raw-ast env
        ($if
          (@-Ast->$-Ast x env)
          (@-Ast->$-Ast y env)
          (@-Ast->$-Ast z env)))]
    #|
    [`(@for ,expr ,var-name ,vec-val)
      (let ([v (@-Ast->$-Ast vec-val)]
            [v-len (match ($-Ast-type v)
                     [(TVectorof _ count) count]
                     [(TVector v) (length v)]
                     ; TODO length?
                     [(TBytes b) b])])
        (new-$-Ast raw-ast env
          (@-Ast->$-Ast
            `(@let (counter ,0)
              (@let (tempvec ,vec-val)
                (@loop ,v-len (set-let
        ;($let (list (gensym 'counter) (new-$-Ast
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
    [`(@ann ,inner ,_) (@-Ast->$-Ast inner)]
    |#
    [`(@lit-bytes ,bts)
      (new-$-Ast raw-ast env ($lit-bytes bts))]
    [`(@block ,es)
      (new-$-Ast raw-ast env ($block (map (lambda ((e : @-Ast))
                                            (@-Ast->$-Ast e env))
                                          es)))]
    [`(@loop ,n ,body)
      (new-$-Ast raw-ast env ($loop n (@-Ast->$-Ast body env)))]
    [other (error "@-ast node is not transformable into $-Ast" other)])))
