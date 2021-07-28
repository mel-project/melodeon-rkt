#lang typed/racket
(require "../common.rkt"
         "../ast-utils.rkt"
         "types.rkt"
         "resolver.rkt"
         "../typed-ast.rkt")
(require racket/hash)

;; Entry point: transforms a whole program
(: @-transform (-> @-Ast $program))
(define (@-transform ast)
  (match (dectx ast)
    [`(@program ,definitions
                ,body)
     ; Stupid, mutation-based approach
     (define type-scope ts-empty)
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
          (match-define (cons $body _) (@->$ body type-scope))
          (define ret-type (if return-type (resolve-type return-type (Type-Scope-type-vars type-scope))
                               ($-Ast-type $body)))
          (unless (subtype-of? ($-Ast-type $body) ret-type)
            (context-error "function ~a annotated with return type ~a but actually returns ~a"
                           (type->string ret-type)
                           (type->string ($-Ast-type $body))))
          (set! type-scope (bind-fun type-scope
                                     name
                                     (TFunction
                                      (map (λ((x : Type-Expr)) (resolve-type x (Type-Scope-type-vars type-scope)))
                                           (map (λ((x : (List Symbol Type-Expr)))
                                                  (second x)) args-with-types))
                                      ($-Ast-type $body))))
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


;; Converts from TVector to TVectorof
(: to-tvector (-> (U TVectorof TVector) TVector))
(define (to-tvector tvec)
  (match tvec
    [(TVectorof inner count)
     (TVector (make-list count inner))]
    [(? TVector? x) x]))

;; Appends two vectors
(: tappend (-> Type Type Type))
(define (tappend left right)
  (match (cons left right)
    [(cons (TBytes n)
           (TBytes m)) (TBytes (+ n m))]
    ;; [T U ..] ++ [V W X ..]
    [(cons (TVector left-types)
           (TVector right-types)) (TVector (append left-types right-types))]
    ;; [T; n] ++ [T; m]
    [(cons (TVectorof left-type left-count)
           (TVectorof right-type right-count))
     (unless (equal? left-type right-type)
       (context-error "tried to append vectors with mismatching types: ~a ++ ~a"
                      left-type
                      right-type))
     (TVectorof left-type (+ left-count right-count))]
    ;; [T T ..] ++ [T; n]
    [(cons (? TVectorU? left)
           (? TVectorU? right))
     (tappend (to-tvector left)
              (to-tvector right))]
    [_
     (context-error "cannot append non-vectors: ~a ++ ~a"
                    left
                    right)]
    ))

;; A function
(struct TFunction ((arg-types : (Listof Type))
                   (result-type : Type)))

;; A type scope
(struct Type-Scope ((vars : Type-Map)
                    (type-vars : Type-Map)
                    (bound-facts : (Immutable-HashTable Symbol Type-Facts))
                    (funs : (Immutable-HashTable Symbol TFunction))) #:prefab)

(: ts-empty Type-Scope)
(define ts-empty (Type-Scope (hash) (hash) (hash) (hash)))

; Return union of two type scopes
(: ts-union (-> Type-Scope Type-Scope Type-Scope))
(define (ts-union x y)
  (Type-Scope
   (hash-union
    (Type-Scope-vars x)
    (Type-Scope-vars y))
   (hash-union
    (Type-Scope-type-vars x)
    (Type-Scope-type-vars y))
   (hash-union
    (Type-Scope-bound-facts x)
    (Type-Scope-bound-facts y))
   (hash-union
    (make-immutable-hash (hash->list (Type-Scope-funs x)))
    (make-immutable-hash (hash->list (Type-Scope-funs y))))))

(: apply-facts (-> Type-Scope Type-Facts Type-Scope))
(define (apply-facts ts tf)
  (foldl
   (lambda ((kv : (Pair Symbol Type))
            (rst : Type-Scope))
     (define corresponding (hash-ref (Type-Scope-vars ts) (car kv) #f))
     (if corresponding
         (bind-var rst
                   (car kv)
                   (if (subtype-of? (cdr kv) corresponding) (cdr kv)
                       (context-error "cannot possibly be of type ~a"
                                      (type->string corresponding))))
         rst))
   ts
   (hash->list tf)))

(: bind-facts (-> Type-Scope Symbol Type-Facts Type-Scope))
(define (bind-facts ts var-name var-facts)
  (match ts
    [(Type-Scope vars type-vars type-facts funs)
     (Type-Scope vars type-vars (hash-set type-facts var-name var-facts) funs)]))


(: bind-var (-> Type-Scope Symbol Type Type-Scope))
(define (bind-var ts var-name var-type)
  (match ts
    [(Type-Scope vars type-vars type-facts funs)
     (Type-Scope (hash-set vars var-name var-type) type-vars type-facts funs)]))

(: bind-fun (-> Type-Scope Symbol TFunction Type-Scope))
(define (bind-fun ts fun-name fun-type)
  (match ts
    [(Type-Scope vars type-vars type-facts funs)
     (Type-Scope vars type-vars type-facts (hash-set funs fun-name  fun-type))]))

(: bind-type-var (-> Type-Scope Symbol Type Type-Scope))
(define (bind-type-var ts var-name var-type)
  (match ts
    [(Type-Scope vars type-vars type-facts funs)
     (Type-Scope vars (hash-set type-vars var-name var-type) type-facts funs)]))

(: lookup-var (-> Type-Scope Symbol Type))
(define (lookup-var ts var-name)
  (or (hash-ref (Type-Scope-vars ts) var-name #f)
      (context-error "undefined variable ~v"
                     (symbol->string var-name))))

(: lookup-type-var (-> Type-Scope Symbol Type))
(define (lookup-type-var ts var-name)
  (or (hash-ref (Type-Scope-type-vars ts) var-name #f)
      (context-error "undefined type ~v"
                     (symbol->string var-name))))

(: lookup-fun (-> Type-Scope Symbol TFunction))
(define (lookup-fun ts var-name)
  (or (hash-ref (Type-Scope-funs ts) var-name #f)
      (context-error "undefined function ~v"
                     (symbol->string var-name))))

;; Resolves a type or throws an error
(: resolve-type (-> Type-Expr Type-Map Type))
(define (resolve-type texpr env)
  (match texpr
    [`(@type-var Any) (TAny)]
    [`(@type-var Bin) (TBin)]
    [`(@type-var Nat) (TNat)]
    ;[`(@type-var ,var) (lookup-type-var 
    ;[`(@type-var ,var) (context-error "cannot resolve type names yet")]
    ;[`(@type-vec ,vec) (TVector (map (lambda (x) (resolve-type x env)) vec))]
    [`(@type-vec ,vec) (TVector (map (λ((x : Type-Expr)) (resolve-type x env)) vec))]
    [`(@type-vecof ,var ,count) (TVectorof (resolve-type var env) count)]
    [`(@type-bytes ,count) (TBytes count)]
    [`(@type-union ,x ,y)
     (TUnion (resolve-type x env)
             (resolve-type y env))]
    [_ (error "wtf man" texpr)]
    ))

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
    [`(@def-fun ,fun ,args ,rettype ,expr)
     (define types-map (Type-Scope-type-vars accum))
     (define inner-scope
       (foldl (λ ((pair : (List Symbol Type-Expr)) (accum : Type-Scope))
                (bind-var
                 accum
                 (first pair)
                 (resolve-type (second pair) types-map)))
              accum
              args))
     (define inner-type (first (@-ast->type/inner expr inner-scope)))
     (: return-type Type)
     (define return-type
       (cond
         [rettype (unless (subtype-of? inner-type (resolve-type rettype types-map))
                    (context-error
                     "~a expected to return type ~a, got type ~a"
                     fun
                     (resolve-type rettype types-map)
                     inner-type))
                  (resolve-type rettype types-map)]
         [else inner-type]))
     (bind-fun accum fun
               (TFunction (map (lambda ((x : Type-Expr))
                                 (resolve-type
                                  x
                                  (Type-Scope-type-vars accum)))
                               (map (inst second Symbol Type-Expr Type-Expr)
                                    args))
                          return-type))]
    [_ ts-empty]))

(: add-def-var (-> Definition Type-Scope Type-Scope))
(define (add-def-var def accum)
  (match def
    [`(@def-var ,var ,expr)
     (bind-var accum var (first (@-ast->type/inner expr accum)))]
    [_ ts-empty]))

(: definitions->scope (-> (Listof Definition) Type-Scope))
(define (definitions->scope defs)
  (let ([struct-defs (foldl add-struct-def (Type-Scope-type-vars ts-empty) defs)]
        [var-defs (foldl add-def-var ts-empty defs)]
        ;[alias-defs (foldl add-alias-def defs)]
        [fun-defs (foldl add-fun-def ts-empty defs)])
    (foldl
     (lambda ((x : (Pairof Symbol Type)) (scope : Type-Scope))
       (bind-type-var scope (car x) (cdr x)))
     (ts-union fun-defs var-defs)
     (hash->list struct-defs))))

;; "smart union" of two types that doesn't create a TUnion if one is the subtype of another
(: smart-union (-> Type Type Type))
(define (smart-union t1 t2)
  (cond
    [(subtype-of? t1 t2) t2]
    [(subtype-of? t2 t1) t1]
    [else (TUnion t1 t2)]))

(define-type Type-Facts (Immutable-HashTable Symbol Type))

(: tf-empty Type-Facts)
(define tf-empty (hash))

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
                                    (pretty-write (list variable tsfacts))
                                    (hash-ref tsfacts variable)]
                                   [else tf-empty])))]
      ;[`(@lit-bytes ,bts) (list (TBytes (bytes-length bts)) (hash))]
      ;; misc
      [`(@extern ,var) (cons ($-Ast (TAny)
                                    ($extern var)) tf-empty)]
      [`(@loop ,count ,body) (let ([body (@->$ body type-scope)])
                               (cons ($-Ast ($type (car body))
                                            ($loop count (car body)))
                                     (cdr body)))]
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
                                   ($bin 'eq (car x)
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
                 ($bin 'append $x $y))
          tf-empty))]

      ;; let expressions
      [`(@let (,val ,expr) ,body)
       (match-define (cons $expr expr-facts) (@->$ expr type-scope))
       (pretty-write (list val expr-facts))
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
       (match-define (cons $sad _) (@->$ sad type-scope)) ; !! TODO !! "negate" the facts
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
        ($-Ast (match ($type $val)
                 [(TVector lst) (unless (< idx (length lst))
                                  (context-error "index ~a out of bounds for vector of ~a elements"
                                                 idx
                                                 (length lst)))
                                (list-ref lst idx)]
                 [(TVectorof t n) (unless (< idx n)
                                    (context-error "index ~a out of bounds for vector of ~a elements"
                                                   idx
                                                   n))
                                  t]
                 [other (context-error "cannot index into a value of type ~a" other)])
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
      ;; TODO: desugar comprehensions
      )))

(module+ test
  (require "../parser.rkt")
  (parameterize ([FILENAME "test.melo"])
    (@-transform
     (melo-parse-port (open-input-string "
(let x = ann 1 : Any in
if x is Nat && x == 10 then
22222222222222
else
    123) / 2
")))))