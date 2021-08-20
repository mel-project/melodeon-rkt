#lang typed/racket
(require "../common.rkt"
         "../ast-utils.rkt"
         "types.rkt"
         "../typed-ast.rkt"
         "typecheck-helpers.rkt"
         "typecheck-unify.rkt")
(require racket/hash)
(require typed-map
         compatibility/defmacro)

(provide @-transform)

;; Entry point: transforms a whole program
(: @-transform (-> @-Ast $program))
(define (@-transform ast)
  (match (dectx ast)
    [`(@program ,initial-defs
                ,body)
     ;(define type-scope (definitions->scope definitions))
     ; Generate accessor fns for all struct types defined
     (define accessor-fn-defs
       (flatten1 (map generate-accessors
                      (filter struct-def? initial-defs))))

     (define definitions (append initial-defs accessor-fn-defs))
     (printf "DEFS\n")
     (pretty-print (sort-topo definitions))
     (define type-scope (definitions->scope definitions))

     ; Stupid, mutation-based approach
     (: $definitions (Listof $fndef))
     (define $definitions '())
     (: $vardefs (Listof $vardef))
     (define $vardefs '())
     (define-macro (snippet)
       '(let ()
          (define inner-type-scope
            (foldl (λ((x : (List Symbol Type-Expr))
                      (ts : Type-Scope))
                     (bind-var ts (first x) (resolve-type (second x) type-scope)))
                   type-scope
                   args-with-types))
          (match-define (cons $body _) (@->$ body inner-type-scope))
          (define ret-type (if return-type (resolve-type return-type type-scope) 
                               ($-Ast-type $body)))
          (unless (subtype-of? ($-Ast-type $body) ret-type)
            (context-error "function ~a annotated with return type ~a but actually returns ~a"
                           (type->string ret-type)
                           (type->string ($-Ast-type $body))))
          (set! $definitions (cons ($fndef name
                                           (map (λ((x : (List Symbol Type-Expr)))
                                                  (list (first x)
                                                        (resolve-type (second x)
                                                                      type-scope)))
                                                args-with-types)
                                           $body)
                                   $definitions))))
     (for ([definition definitions])
       (match definition
         [`(@def-fun ,name
                     ,args-with-types
                     ,return-type
                     ,body)
          (snippet)]
         [`(@def-generic-fun ,name
                             ,_
                             ,args-with-types
                             ,return-type
                             ,body)
          (snippet)]
         [`(@def-var ,name ,body)
          (match-define (cons $body _) (@->$ body type-scope))
          (set! $vardefs (cons ($vardef name $body) $vardefs))]
         [_ (void)]))
     ($program $definitions
               $vardefs
               (car (@->$ body type-scope)))]))

; When given a @def-struct, generates a list of @def-fun's
; to access every field of the struct.
(: generate-accessors (-> Definition (Listof Definition)))
(define (generate-accessors def)
  (match def
    [`(@def-struct ,struct-name ,binds)
     (map (λ ((tuple : (List Integer (List Symbol Type-Expr))))
            (match-define (cons i (cons field texpr)) tuple)
            `(@def-fun
              ; TODO mangle
              ,(accessor-name struct-name (car field))
              ;,(string->symbol (format "~a-~a" struct-name (car field)))
              ((@x (@type-struct ,struct-name ,binds)))
              #f
              ; Add one to index i bcs first is always the struct id
              (@index (@var @x) (@lit-num ,(cast (+ i 1) Nonnegative-Integer)))))
          (enumerate binds))]
    [_ '()]))

; Assign a number to each element of a list
(: enumerate (All (T) (-> (Listof T) (Listof (List Integer T)))))
(define (enumerate l)
  (foldl (λ ((x : T) (acc : (Listof (List Integer T))))
           (cons
            (list (+ 1 (caar acc))
                  x)
            acc))
         (list (list 0 (car l)))
         (cdr l)))


; Extract a list of names that a given @-Ast refers to (depends
; on) and append them to the given list if they are not already
; in it.
;(: sort-topo-fold (All (A) (-> @-Ast (Listof A) (Listof A))))
(: sort-topo-fold (-> @-Ast (Listof Symbol) (Listof Symbol)))
(define (sort-topo-fold ast names)
  (define if-new (λ ((name : Symbol)) : (Listof Symbol)
    (if (member name names) '() (list name))))

  (flatten1 (list
    (flatten1 (ast-list-map
      (λ ((ast : @-Ast))
        (match (dectx ast)
          [`(@apply ,name ,_) (if-new (cast name Symbol))]
          [`(@instantiate ,name ,_) (if-new (cast name Symbol))]
          [_ '()]))
      ast))
    names)))

; TODO
;(: def-sort-topo-fold (-> Definition (Listof Symbol) (Listof Symbol)))

; Map a list of definitions to their inner @-Ast expression
; which may refer to (depend on) other definitions. Then fold
; over the expressions to build a list of definition names,
; ordered by dependency. Finally map names to the original
; definitions again.
(: sort-topo (-> (Listof Definition) (Listof Definition)))
(define (sort-topo defs)
  (define get-def-with-name
    (λ (name)
       (car (filter (λ (def)
       ;(findf
         ;(λ (def) (match def
         (match def
           [`(@def-struct ,n ,_) (equal? n name)]
           [`(@def-fun ,n ,_ ,_ ,_) (equal? n name)]
           [_ #f]))
         defs))))

  (map get-def-with-name
       (foldr sort-topo-fold
              '()
              ; Extract inner @-Ast expressions from definitions
              ; which can used to determine dependencies
              (cast (filter (compose not empty?) (map
                (λ (def) (match def
                  [`(@def-generic-fun ,_ ,_ ,_ ,_ ,body) body]
                  ;[`(@def-struct _ _) '()]
                  ;[`(@def-alias _ _) '()]
                  [`(@def-fun ,_ ,_ ,_ ,body) body]
                  [_ '()]))
                defs)) (Listof @-Ast)))))

; Convert a string to the sum of its ascii
; character encodings
;(: string->uint (-> String Nonnegative-Integer))
(: string->uint (-> String Integer))
(define (string->uint s)
  ;(foldl (λ ((c : Char) (acc : Nonnegative-Integer))
  (foldl (λ ((c : Char) (acc : Integer))
           ;(foldl (λ (c acc)
           ;(ann (char->integer c) Nonnegative-Integer))
           ;(cast (+ acc (char->integer c)) Nonnegative-Integer))
           (+ acc (char->integer c)))
         0
         (string->list s)))

; Search type-vars in a type scope and return the
; name of the first matching type
(: find-name-by-type (-> Type Type-Scope (Option Symbol)))
(define (find-name-by-type type ts)
  (foldl (λ (tup name)
           (if (and (equal? type (cdr tup)) (not name))
               (car tup)
               name))
         #f
         (hash->list (Type-Scope-type-vars ts))))

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
                                             (= num 1)) (TNat) (TNat))
                                     ($lit-num num))
                              tf-empty)]
      [`(@lit-vec ,vars) (define $vars (map (λ((a : @-Ast)) (car (@->$ a type-scope))) vars))
                         (cons ($-Ast
                                (TVector (map $type $vars))
                                ($lit-vec $vars))
                               tf-empty)]
      [`(@init-vec ,expr ,size)
       ; TODO use this once we have constant generics?
       ;(define $size (@->$ size type-expr))
       ;(unless (not (subtype-of? ($type $size) (TNat)))
       ;  (context-error "Expected size to be a subtype of Nat in vector
       ;                  initialization, but its a ~a" ($type $size))
       (match-define (cons $expr _) (@->$ expr type-scope))
       (cons ($-Ast
              (TVectorof ($type $expr) size)
              ($init-vec $expr size))
             tf-empty)]
      [`(@lit-bytes ,bts) (cons ($-Ast
                                 (TBytes (bytes-length bts))
                                 ($lit-bytes bts))
                                tf-empty)]
      [`(@var ,variable) (cons ($-Ast (lookup-var type-scope variable)
                                      ($var variable))
                               (let ([tsfacts (Type-Scope-bound-facts type-scope)])
                                 (cond
                                   [(hash-has-key? tsfacts variable)
                                    (hash-ref tsfacts variable)]
                                   [else tf-empty])))]
      ;[`(@lit-bytes ,bts) (list (TBytes (bytes-length bts)) (hash))]
      [`(@accessor ,var ,field)
       ; TODO check that the type has the requested field
       (let ([$var (car (@->$ var type-scope))])
         (match ($-Ast-type $var)
           [(TTagged tag types)
            (@->$
             `(@apply ,(accessor-name tag field) (,var))
             type-scope)]
           [_ (context-error "When accessing field '~a' expected '~a' to be a
                              struct type, but it's actually a ~a" field var
                                                                   ($-Ast-type $var))]))]
      [`(@instantiate ,type-name ,args)
       (let ([type (lookup-type-var type-scope type-name)])
         (cons (match type
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

                    ($-Ast type ;(TVector (cons (TNat) types))
                           ($lit-vec (cons
                                      ($-Ast
                                       (TNat)
                                       ($lit-num (cast (string->uint
                                                        (symbol->string type-name))
                                                       Nonnegative-Integer)))
                                      (map (λ ((x : (Pairof $-Ast Type-Map))) : $-Ast
                                             (car x)) $args)))))]
                 [_ (context-error "~a is not a custom type which can be instantiated."
                                   type-name)])
               tf-empty))]
      ;; misc
      [`(@extern ,var) (cons ($-Ast (TAny)
                                    ($extern var)) tf-empty)]
      [`(@extern-call ,fname ,args) (cons ($-Ast (TAny)
                                                 ($extern-call fname
                                                               (for/list ([arg args]) : (Listof $-Ast)
                                                                 (car (@->$ arg type-scope)))))
                                          tf-empty)]
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
                      (cons ($-Ast (TNat)
                                   ($eq (car x)
                                        (car y)))
                            tf-empty))]
      [`(,(? (lambda(op) (member op '(@band @bor @shl @shr @xor @+ @- @* @/))) op) ,x ,y)
       (match-let ([(cons $x _) (@->$ x type-scope)]
                   [(cons $y _) (@->$ y type-scope)])
         (define x-type ($type $x))
         (define y-type ($type $x))
         (define generate-vec
           (λ ((l : (Listof $-Ast))
               (inner-types : (Listof Type))
               (n : Nonnegative-Integer))
            (if (eq? 1 (length l))
              ($-Ast (TVectorof (car inner-types) n)
                     ($lit-vec (make-list n (car l))))
              ($-Ast (TVectorof (TVector inner-types) n)
                     ($lit-vec (cast (make-list n l) (Listof $-Ast)))))))

         ; vector multiply syntax
         (cons (match (cons $x $y)
           [(cons ($-Ast (TVector inner-types) ($lit-vec l)) ($-Ast _ ($lit-num n)))
            (generate-vec l inner-types n)]
           [(cons ($-Ast _ ($lit-num n)) ($-Ast (TVector inner-types) ($lit-vec l)))
            (generate-vec l inner-types n)]
           #|[(cons ($-Ast _ ($var var)) ($-Ast (TVector inner-types) ($lit-vec l)))
            (let (n (type-scope-vars var))
              (generate-vec l inner-types n))]|#
           [_
             (assert-type x ($type $x) (TNat))
             (assert-type y ($type $y) (TNat))
              ($-Ast (TNat)
                     ($bin (match op
                             ['@bor 'or]
                             ['@band 'and]
                             ['@xor 'xor]
                             ['@shl 'shl]
                             ['@shr 'shr]
                             ['@+ '+]
                             ['@- '-]
                             ['@* '*]
                             ['@/ '/])
                           $x
                           $y))])
         tf-empty))]
      [`(@and ,x ,y)
       ; trivial desugaring
       (@->$ `(@let (@x ,x)
                    (@if (@var @x)
                         ,y
                         (@var @x)))
             type-scope)]
      [`(@or ,x ,y)
       ; trivial desugaring
       (@->$ `(@let (@x ,x)
                    (@if (@var @x)
                         (@var @x)
                         ,y))
             type-scope)]
      [`(@append ,x ,y)
       ; no type facts can possibly propagate out of an @append
       (match-let ([(cons $x _) (@->$ x type-scope)]
                   [(cons $y _) (@->$ y type-scope)])
         (cons
          ($-Ast (type-append ($type $x)
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
       (define new-type (resolve-type type type-scope))
       (unless (subtype-of? new-type ($type $inner))
         (context-error "cannot downcast ~a to ~a"
                        (type->string ($type $inner))
                        (type->string new-type)))
       (cons (match $inner
               [($-Ast _ node) ($-Ast new-type node)])
             inner-facts)]
      [`(@ann ,inner ,type)
       (match-define (cons $inner inner-facts) (@->$ inner type-scope))
       (define new-type (resolve-type type type-scope))
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
       (match-define (cons $sad _) (@->$ sad (subtract-facts type-scope facts))) ; !! TODO !! "negate" the facts
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
      [`(@range ,val ,from-expr ,to-expr)
       ;(: idx Nonnegative-Integer)
       (define to-idx (λ ((expr : @-Ast))
                        (match (dectx expr)
                          [`(@lit-num ,x) x]
                          [other (context-error "non-literal index ~a not yet supported"
                                                other)])))
       (define to (to-idx to-expr))
       (define from (to-idx from-expr))
       (match-define (cons $val _) (@->$ val type-scope))
       (cons
        ($-Ast (type-index ($type $val) to)
               ($range $val
                       ($-Ast (TNat) ($lit-num from))
                       ($-Ast (TNat) ($lit-num to))))
        tf-empty)]
      [`(@apply ,fun ,args)
       (define $args
         (map (λ((a : @-Ast)) (car (@->$ a type-scope))) args))
       (define my-arg-types (map $type $args))
       (match ((lookup-fun type-scope fun) my-arg-types)
         [(TFunction arg-types result)
          (unless (equal? (length arg-types) (length args))
            (error '@-ast->type "[~a] calling function ~a with ~a arguments instead of the expected ~a"
                   (context->string (get-context @-ast))
                   fun
                   (length args)
                   (length arg-types)))

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
        ($-Ast (TNat)
               ($is (car (@->$ expr type-scope))
                    (resolve-type type type-scope)))
        (match (dectx expr)
          [`(@var ,var)
           (make-immutable-hash `((,var . ,(resolve-type type type-scope))))]
          [else tf-empty]))]
      )))

(: empty-ts Type-Scope)
(define empty-ts (Type-Scope (hash) (hash) (hash) (hash)))

(: type-map-set (All (a b) (-> (Immutable-HashTable a b) a b (Immutable-HashTable a b))))
(define (type-map-set hash k v) (hash-set hash k v))

; Read a Definition ast node and if a struct definition,
; add to the given type map. The given type map is also
; used to potentitally resolve type variables in the struct.
(: add-struct-def (-> Definition Type-Scope Type-Scope))
(define (add-struct-def def env)
  (match def
    [`(@def-struct ,name ,fields)
     (bind-type-var
      env
      name
      (resolve-type `(@type-struct ,name ,fields) env))]
    [_ env]))

; Read a Definition ast node and if an alias definition,
; add to the given type map. The given type map is also
; used to potentitally resolve type variables in the struct.
(: add-alias-def (-> Definition Type-Scope Type-Scope))
(define (add-alias-def def env)
  (match def
    [`(@def-alias ,name ,whatever)
     (bind-type-var
      env
      name
      (resolve-type whatever env))]
    [_ env]))

; takes a Type-Scope rather than just one map because
; types may need to be resolved and the function-scope
; should also be added to.
(: add-fun-def (-> Definition Type-Scope Type-Scope))
(define (add-fun-def def accum)
  ;; stupid hack
  (define-macro (snippet)
    '(parameterize ([current-context (context-of body)])
       (define inner-type-scope
         (foldl (λ((x : (List Symbol Type-Expr))
                   (ts : Type-Scope))
                  (bind-var ts (first x)
                            (resolve-type (second x) accum)))
                accum
                args-with-types))
       (match-define (cons $body _) (@->$ body inner-type-scope))
       (define ret-type (if return-type
                            (resolve-type return-type accum)
                            ($-Ast-type $body)))
       (unless (subtype-of? ($-Ast-type $body) ret-type)
         (context-error "function ~a annotated with return type ~a but actually returns ~a"
                        name
                        (type->string ret-type)
                        (type->string ($-Ast-type $body))))
       (TFunction
        (map (λ((x : Type-Expr))
               (resolve-type x accum))
             (map (λ((x : (List Symbol Type-Expr)))
                    (second x)) args-with-types))
        ($-Ast-type $body))))
  (match def
    [`(@def-fun ,name
                ,args-with-types
                ,return-type
                ,body)
     ;; A "noop" generic type
     (bind-fun accum
               name
               (λ _ (snippet)))]
    [`(@def-generic-fun ,name
                        ,generic-params
                        ,args-with-types
                        ,return-type
                        ,body)
     (define tf-with-params (snippet))
     ;; do unification here
     (bind-fun accum
               name
               (lambda ((callsite-arg-types : (Listof Type)))
                 (match tf-with-params
                   [(TFunction arg-types result-type)
                    (define unification-table
                      (for/fold ([accum : (Immutable-HashTable TVar Type) (hash)])
                                ([arg-type arg-types]
                                 [callsite-arg-type callsite-arg-types])
                        (hash-union accum
                                    (type-unify arg-type callsite-arg-type))))
                    (TFunction (map (λ((x : Type))
                                      (type-template-fill x unification-table))
                                    arg-types)
                               (type-template-fill result-type unification-table))
                                      ])))]
     
    [_ accum]))
     

(: add-var-def (-> Definition Type-Scope Type-Scope))
(define (add-var-def def accum)
  (match def
    [`(@def-var ,var ,expr)
     (bind-var accum var ($-Ast-type (car (@->$ expr accum))))]
    [_ accum]))

(: definitions->scope (-> (Listof Definition) Type-Scope))
(define (definitions->scope defs)
  (for/fold ([accum empty-ts])
            ([def defs]) : Type-Scope
    (add-fun-def def (add-var-def def (add-struct-def def (add-alias-def def accum))))))

(module+ test
    (require "../parser.rkt")
    (parameterize ([FILENAME "test.melo"])
      (time
       (@-transform
        (melo-parse-port (open-input-string "
def labooyah() = 2
def dup<T>(x: T) = [x, x]
def getfirst<T>(x : [T, T]) = x[0]
def roundtrip<T>(x: T) = getfirst(dup(x))
- - - 
roundtrip([1, 2, 3])[0] + roundtrip(5) + 6
"))))))