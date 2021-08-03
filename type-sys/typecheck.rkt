#lang typed/racket
(require "../common.rkt"
         "../ast-utils.rkt"
         "types.rkt"
         "resolver.rkt"
         "../typed-ast.rkt"
         "typecheck-helpers.rkt"
         "typecheck-unify.rkt")
(require racket/hash)
(provide @-transform)

;; Entry point: transforms a whole program
(: @-transform (-> @-Ast $program))
(define (@-transform ast)
  (match (dectx ast)
    [`(@program ,definitions
                ,body)
     (define type-scope (definitions->scope definitions))
     ; Stupid, mutation-based approach
     (: $definitions (Listof $fndef))
     (define $definitions '())
     (: $vardefs (Listof $vardef))
     (define $vardefs '())
     (for ([definition definitions])
       (match definition
         [`(@def-fun ,name
                     ,args-with-types
                     ,return-type
                     ,body)
          (define inner-type-scope
            (foldl (λ((x : (List Symbol Type-Expr))
                      (ts : Type-Scope))
                     (bind-var ts (first x) (resolve-type (second x) (Type-Scope-type-vars type-scope))))
                   type-scope
                   args-with-types))
          (match-define (cons $body _) (@->$ body inner-type-scope))
          (define ret-type (if return-type (resolve-type return-type (Type-Scope-type-vars type-scope))
                               ($-Ast-type $body)))
          (unless (subtype-of? ($-Ast-type $body) ret-type)
            (context-error "function ~a annotated with return type ~a but actually returns ~a"
                           (type->string ret-type)
                           (type->string ($-Ast-type $body))))
          (set! $definitions (cons ($fndef name
                                           (map (λ((x : (List Symbol Type-Expr)))
                                                  (list (first x)
                                                        (resolve-type (second x)
                                                                      (Type-Scope-type-vars type-scope))))
                                                args-with-types)
                                           $body)
                                   $definitions))]
         [`(@def-var ,name ,body)
          (match-define (cons $body _) (@->$ body type-scope))
          (set! $vardefs (cons ($vardef name $body) $vardefs))]
         [_ (void)]))
     ($program $definitions
               $vardefs
               (car (@->$ body type-scope)))]))


; Assign a number to each element of a list
#|
(: enumerate (All (T) (-> (Listof T) (Listof (List Nonnegative-Integer T)))))
(define (enumerate l)
  (foldl (λ ((x : T) (acc : (List Nonnegative-Integer (Listof T))))
            : (List Nonnegative-Integer T)
            (list (+ 1 (car acc))
                  (cons x (cadr acc))))
         (list 0 (car l))
         (cdr l)))
|#

;(: @-ast->type/inner (-> @-Ast Type-Scope (List (Option Type) Type-Facts)))

(: @-ast->type/inner (-> @-Ast Type-Scope (List Type Type-Facts)))
(define (@-ast->type/inner @-ast type-scope) (error "placeholder"))

;; This function is the "meat" of the typechecker.
;;    Given a @-Ast and a type scope, it returns:
;;    - A type-annotated $-Ast
;;    - *Type facts if true*: what this $-Ast evaluating to a truthy value means for other types.
;;       For example, "(x is Nat && y is Nat) || (foobar() && y is Nat)" being true would imply that y must belong to Nat.
(: @->$ (-> @-Ast Type-Scope
            (Pair $-Ast Type-Facts)))
(define (@->$ @-ast type-scope)
  ;; assert the type, using the current scope
  (: assert-type (-> @-Ast Type Type Void))
  (define (assert-type @-ast real-type type)
    (parameterize ([current-context (context-of @-ast)])
      (unless (subtype-of? real-type type)
        (context-error
         "expected type ~a, got type ~a"
         (type->string type)
         (type->string real-type)))))
  
  ;; shorthand
  (define $type $-Ast-type)

  (define types-map (Type-Scope-type-vars type-scope))
  (parameterize ([current-context (context-of @-ast)])
    (match (dectx @-ast)
      ;; literals
      [`(@lit-num ,num) (cons ($-Ast (if (or (= num 0)
                                             (= num 1)) (TBin) (TNat))
                                     ($lit-num num))
                              tf-empty)]
      [`(@lit-vec ,vars) (define $vars (map (λ((a : @-Ast)) (car (@->$ a type-scope))) vars))
                         (cons ($-Ast
                                (TVector (map $type $vars))
                                ($lit-vec $vars))
                               tf-empty)]
      [`(@var ,variable) (cons ($-Ast (lookup-var type-scope variable)
                                      ($var variable))
                               (let ([tsfacts (Type-Scope-bound-facts type-scope)])
                                 (cond
                                   [(hash-has-key? tsfacts variable)
                                    (hash-ref tsfacts variable)]
                                   [else tf-empty])))]
      ;[`(@lit-bytes ,bts) (list (TBytes (bytes-length bts)) (hash))]
      [`(@instantiate ,type-name ,args)
        (let ([type (lookup-type-var type-scope type-name)])
          (cons (match type
            ;[`(@type-struct ,_ ,params)
            [(TTagged _ types)
              (let ([$args (map (λ ((arg : @-Ast)) (@->$ arg type-scope)) args)])
                ; Check that arg-types match parameter types, or throw error
                (for ([arg (map (λ ((x : (Pairof $-Ast Type-Map))) (car x)) $args)]
                      [param-type types])
                  (unless (equal? ($-Ast-type arg) param-type)
                    ; TODO print the @ast for a cleaner repr, or just the index
                    (context-error "Argument of instantiated type is of type ~a but type ~a is expected."
                                   (type->string ($-Ast-type arg))
                                   (type->string param-type))))

                ($-Ast (TVector (cons (TNat) types))
                       ($lit-vec (cons
                                   ($-Ast (TNat) ($lit-num 0))
                                   (map (λ ((x : (Pairof $-Ast Type-Map))) : $-Ast
                                           (car x)) $args)))))]
                       ;($lit-vec (map (λ ((x : (List Symbol Type))) : Type (cadr x)) args)))]
            [_ (context-error "~a is not a custom type which can be instantiated."
                              type-name)])
            tf-empty))]
      ;; misc
      [`(@extern ,var) (cons ($-Ast (TAny)
                                    ($extern var)) tf-empty)]
      [`(@loop ,count ,body) (let ([body (@->$ body type-scope)])
                               (cons ($-Ast ($type (car body))
                                            ($loop count (car body)))
                                     (cdr body)))]
      [`(@for ,expr ,var-name ,vec-expr)
        (letrec
          ; TODO don't pass along type-facts in vec-pair
          ([$vec-pair (@->$ vec-expr type-scope)]
           [$vec-expr (car $vec-pair)]
           [vec-type ($-Ast-type $vec-expr)])
          ; TODO should this somehow check if vec-type is a subtype of some
          ; kind of general vector?
          (match vec-type
            ; If mapping on a Vectorof, return an $-Ast
            [(TVectorof inner-type count)
             (letrec
               ([inner-ts (bind-var type-scope var-name inner-type)]
                [$expr (car (@->$ expr inner-ts))])
               ; Return an $-Ast
               (cons ($-Ast
                       (TVectorof ($-Ast-type $expr) count)
                       ($for $expr var-name $vec-expr))
                     tf-empty))]
            [(? TVector? tvec)
             (letrec
               ([inner-type (tvector-inner-type tvec)]
                [inner-ts (bind-var type-scope var-name inner-type)]
                [$expr (car (@->$ expr inner-ts))])
                (cons ($-Ast
                        (TVectorof ($-Ast-type $expr)
                                   (length (TVector-lst tvec)))
                        ($for $expr var-name $vec-expr))
                      tf-empty))]
            ; Otherwise error
            [(var t) (context-error "vector comprehension needs to iterate
                                    over a vector, but a ~a was
                                    provided"
                                    (type->string t))]))]
      [`(@block ,body)
       (: transformed-body (Listof (Pair $-Ast Type-Facts)))
       (define transformed-body
         (map (λ((x : @-Ast)) (@->$ x type-scope))
              body))
       (define last-type ($type (car (car (reverse transformed-body)))))
       (cons ($-Ast last-type
                    ($block (map (λ((x : (Pair $-Ast Type-Facts))) (car x)) transformed-body)))
             tf-empty)]

      ;; binary operations
      [`(@eq ,x ,y) (let ([x (@->$ x type-scope)]
                          [y (@->$ y type-scope)])
                      (cons ($-Ast (TBin)
                                   ($eq (car x)
                                        (car y)))
                            tf-empty))]
      [`(,(? (lambda(op) (member op '(@+ @- @* @/))) op) ,x ,y)
       (match-let ([(cons $x _) (@->$ x type-scope)]
                   [(cons $y _) (@->$ y type-scope)])
         (assert-type x ($type $x) (TNat))
         (assert-type y ($type $y) (TNat))
         (cons
          ($-Ast (TNat)
                 ($bin (match op
                         ['@+ '+]
                         ['@- '-]
                         ['@* '*]
                         ['@/ '/])
                       $x
                       $y))
          tf-empty))]
      [`(@and ,x ,y)
       ; trivial desugaring
       (define tmp (gensym 'and))
       (@->$ `(@let (,tmp ,x)
                    (@if (@var ,tmp)
                         ,y
                         (@var ,tmp)))
             type-scope)]
      [`(@or ,x ,y)
       ; trivial desugaring
       (define tmp (gensym 'and))
       (@->$ `(@let (,tmp ,x)
                    (@if (@var ,tmp)
                         (@var ,tmp)
                         ,y))
             type-scope)]
      [`(@append ,x ,y)
       ; no type facts can possibly propagate out of an @append
       (match-let ([(cons $x _) (@->$ x type-scope)]
                   [(cons $y _) (@->$ y type-scope)])
         (cons
          ($-Ast (tappend ($type $x)
                          ($type $y))
                 ($append $x $y))
          tf-empty))]

      ;; let expressions
      [`(@let (,val ,expr) ,body)
       (match-define (cons $expr expr-facts) (@->$ expr type-scope))
       (match-define (cons $body body-facts)
         (@->$ body (bind-facts (bind-var type-scope val ($type $expr))
                                val expr-facts)))
       (cons ($-Ast ($type $body)
                    ($let val
                          $expr
                          $body))
             (hash-remove body-facts val))]

      ;; downcast and upcast
      [`(@unsafe-cast ,inner ,type)
       (match-define (cons $inner inner-facts) (@->$ inner type-scope))
       (define new-type (resolve-type type types-map))
       (unless (subtype-of? new-type ($type $inner))
         (context-error "cannot downcast ~a to ~a"
                        (type->string ($type $inner))
                        (type->string new-type)))
       (cons (match $inner
               [($-Ast _ node) ($-Ast new-type node)])
             inner-facts)]
      [`(@ann ,inner ,type)
       (match-define (cons $inner inner-facts) (@->$ inner type-scope))
       (define new-type (resolve-type type types-map))
       (unless (subtype-of? ($type $inner) new-type)
         (context-error "cannot annotate ~a as incompatible type ~a"
                        (type->string ($type $inner))
                        (type->string new-type)))
       (cons (match $inner
               [($-Ast _ node) ($-Ast new-type node)])
             inner-facts)]

      ;; if
      [`(@if ,cond ,happy ,sad)
       (match-define (cons $cond facts) (@->$ cond type-scope)) ; just to check
       (match-define (cons $happy _) (@->$ happy (apply-facts type-scope facts)))
       (match-define (cons $sad _) (@->$ sad (apply-facts type-scope (tf-negate facts)))) ; !! TODO !! "negate" the facts
       (cons ($-Ast (smart-union ($type $happy)
                                 ($type $sad))
                    ($if $cond $happy $sad))
             tf-empty)]

      ;; index and apply
      [`(@index ,val ,idx-expr)
       (: idx Nonnegative-Integer)
       (define idx
         (match (dectx idx-expr)
           [`(@lit-num ,x) x]
           [other (context-error "non-literal index ~a not yet supported" other)]))
       (match-define (cons $val _) (@->$ val type-scope))
       (cons
        ($-Ast (type-index ($type $val)
                idx)
               ($index $val ($-Ast (TNat) ($lit-num idx))))
        tf-empty)]
      [`(@apply ,fun ,args)
       (match (lookup-fun type-scope fun)
         [(TFunction arg-types result)
          (unless (equal? (length arg-types) (length args))
            (error '@-ast->type "[~a] calling function ~a with ~a arguments instead of the expected ~a"
                   (context->string (get-context @-ast))
                   fun
                   (length args)
                   (length arg-types)))
          (define $args
            (map (λ((a : @-Ast)) (car (@->$ a type-scope))) args))
          (for ([arg args]
                [$arg $args]
                [arg-type arg-types])
            (assert-type arg ($type $arg) arg-type))
          (cons ($-Ast result
                       ($apply fun $args))
                tf-empty)]
                       
         [_ (error '@-ast->type "[~a] undefined function ~a"
                   (context->string (get-context @-ast)) fun)])]
      [`(@is ,expr ,type)
       (cons
        ($-Ast (TBin)
               ($is (car (@->$ expr type-scope))
                    (resolve-type type (Type-Scope-type-vars type-scope))))
        (match (dectx expr)
          [`(@var ,var)
           (make-immutable-hash `((,var . ,(resolve-type type types-map))))]
          [else tf-empty]))]
      )))

(: empty-ts Type-Scope)
(define empty-ts (Type-Scope (hash) (hash) (hash) (hash)))

; Read a Definition ast node and if a struct definition,
; add to the given type map. The given type map is also
; used to potentitally resolve type variables in the struct.
(: add-struct-def (-> Definition Type-Map Type-Map))
(define (add-struct-def def env)
  (match def
    [`(@def-struct ,name ,fields)
      (hash-set
        env
        name
        (TTagged
          name
          (map (lambda ([x : (List Symbol Type-Expr)])
                 (resolve-type (cadr x) env))
               fields)))]
    [_ (make-immutable-hash '())]))

; takes a Type-Scope rather than just one map because
; types may need to be resolved and the function-scope
; should also be added to.
(: add-fun-def (-> Definition Type-Scope Type-Scope))
(define (add-fun-def def accum)
  (match def
    [`(@def-fun ,name
                ,args-with-types
                ,return-type
                ,body)
     (define inner-type-scope
       (foldl (λ((x : (List Symbol Type-Expr))
                 (ts : Type-Scope))
                (bind-var ts (first x)
                          (resolve-type (second x) (Type-Scope-type-vars accum))))
              accum
              args-with-types))
     (match-define (cons $body _) (@->$ body inner-type-scope))
     (define ret-type (if return-type
                          (resolve-type return-type (Type-Scope-type-vars accum))
                          ($-Ast-type $body)))
     (unless (subtype-of? ($-Ast-type $body) ret-type)
       (context-error "function ~a annotated with return type ~a but actually returns ~a"
                      (type->string ret-type)
                      (type->string ($-Ast-type $body))))
     (bind-fun accum
       name
       (TFunction
        (map (λ((x : Type-Expr))
               (resolve-type x (Type-Scope-type-vars accum)))
             (map (λ((x : (List Symbol Type-Expr)))
                    (second x)) args-with-types))
        ($-Ast-type $body)))]
    [_ empty-ts]))

(: add-def-var (-> Definition Type-Scope Type-Scope))
(define (add-def-var def accum)
  (match def
    [`(@def-var ,var ,expr)
      (bind-var accum var (first (@-ast->type/inner expr accum)))]
    [_ empty-ts]))

(: definitions->scope (-> (Listof Definition) Type-Scope))
(define (definitions->scope defs)
  (let ([struct-defs (foldl add-struct-def (Type-Scope-type-vars empty-ts) defs)]
        [var-defs (foldl add-def-var empty-ts defs)]
        ;[alias-defs (foldl add-alias-def defs)]
        [fun-defs (foldl add-fun-def empty-ts defs)])
    (foldl
      (lambda ((x : (Pairof Symbol Type)) (scope : Type-Scope))
        (bind-type-var scope (car x) (cdr x)))
      (ts-union fun-defs var-defs)
      (hash->list struct-defs))))

(module+ test
  (require "../parser.rkt")
  (parameterize ([FILENAME "test.melo"])
    (time
     (@-transform
      (melo-parse-port (open-input-string "
def dup(x: Nat) = [x, x]
def trip(x: Nat) = [x, x, x]
- - - 
(let x = if 1 then dup(1) else trip(1) in
if x is [Nat, Nat] then
    trip(dup(x[0])[0] * 100)[0]
else
    dup(x[2])[1]) + 1
"))))))