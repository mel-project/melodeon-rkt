#lang typed/racket
(require "../asts/raw-ast.rkt"
         "../asts/ast-utils.rkt"
         "types.rkt"
         "type-bag.rkt"
         "../asts/typed-ast.rkt"
         "../asts/topo-sort.rkt"
         "typecheck-helpers.rkt"
         "typecheck-unify.rkt")
(require racket/hash)
(require typed-map
         compatibility/defmacro)

(provide @-transform)


;; Sorts all the definitions in a $program
(: $fndefs-sort
   (-> (Listof $fndef)
       (Listof $fndef)))
(define ($fndefs-sort definitions)
  ;; define a parent hashtable
  (: parents-map (HashTable $fndef (Listof $fndef)))
  (define parents-map (make-hasheq))
  (for ([def definitions])
    (hash-set! parents-map def
               (cast
                (filter (λ (x) x)
                        (map (λ(name) (find-$def-by-name name definitions))
                             (set->list ($def-parents def))))
                (Listof $fndef))))
  ;; go through the whole thing
  (: result (Listof $fndef))
  (define result '())
  (: seen (Setof $fndef))
  (define seen (seteq))
  (: emit (-> $fndef Void))
  (define (emit def)
    (define parents (hash-ref parents-map def (λ () '())))
    (unless (set-member? seen def)
      (set! seen (set-add seen def))
      (unless (set-member? seen def)
        (error "Wtf?"))
      (for-each emit parents)
      (set! result (cons def result))))
  (for ([def definitions])
    (emit def))
  (reverse result))

;; Sorts all the definitions in a @program
(: definitions-sort
   (-> (Listof Definition)
       (Listof Definition)))
(define (definitions-sort definitions)
  ;; define a parent hashtable
  (: parents-map (HashTable Definition (Listof Definition)))
  (define parents-map (make-hasheq))
  (for ([def definitions])
    (hash-set! parents-map def
               (append*
                (map (λ(name) (find-defs-by-name name definitions))
                     (set->list (@def-parents def))))))
  ;; go through the whole thing
  (: result (Listof Definition))
  (define result '())
  (: seen (Setof Definition))
  (define seen (seteq))
  (: emit (-> Definition Void))
  (define (emit def)
    (define parents (hash-ref parents-map def (λ () '())))
    (unless (set-member? seen def)
      (set! seen (set-add seen def))
      (unless (set-member? seen def)
        (error "Wtf?"))
      (for-each emit parents)
      (set! result (cons def result))))
  (for ([def definitions])
    (emit def))
  (reverse result))

; Given a type with a constant expression of
; a single variable (i.e. 'N), substitute with
; the constant expression of another type.
(: subst-const-expr-type (-> Type Type Type))
(define (subst-const-expr-type t sub)
  ; check that e is a single variable
  (define assert-var-expr (λ(e)
                            (unless (symbol? e)
                              (context-error "only single variables are supported in
                     constant generic parameters, ~a was provided"
                                             e))))

  (match (cons t sub)
     ; TODO not checking whether types match it
    [(cons (TVectorof it e) (TVector types))
     (define len (length types))
     (assert-var-expr e)
     (TVectorof it (subst-const-expr e e len))]
    [(cons (TVectorof it e) (TVectorof _ sub-e))
     (assert-var-expr e)
     (TVectorof it (subst-const-expr e e sub-e))]
    [(cons (TBytes e) (TBytes sub-e))
     (assert-var-expr e)
     (TBytes (subst-const-expr e e sub-e))]
    [_ t]))

; Solves for a list of variables by setting
; the two lists of const exprs equal to eachother.
; The expressions may correspond to parameters and
; arguments of a function.
(: solve-const-exprs
   (-> (Listof Const-Expr)
       (Listof Symbol)
       (Listof Const-Expr)
       (HashTable Symbol Const-Expr)))
(define (solve-const-exprs lhs vars rhs)
  ; TODO
  ; mangle rhs variables to avoid conflict with lhs
  ;(define mangled-rhs (map (λ(e) (format rhs))

  ; TODO check that all vars are assigned an expr in the result map

  ; Accumulate a mapping of vars to their expression
  (foldl
    (λ(expr-pair var-map)
      ; TODO
      ; get all variables in the left expr,
      ; there should only be one
      ;(letrec ([vars (const-expr-vars l)]
               ;[var (car vars)])
        ; Eventually we solve for the var
        ; But for now expect the left expr to just be a var
      (match-define (cons l r) expr-pair)
      (unless (symbol? l)
        (context-error "Only single-variable constant expressions are
                       supported in parameter types right now. ~a was
                       provided" (const-expr->string l)))

      ; If var is already in the map, check that its the same
      (let ([l-val (hash-ref var-map l #f)])
        (if l-val
          (if (not (equal? l-val r))
            (context-error "Conflicting constant expressions. ~a is expected
                           to be both ~a and ~a" l l-val r)
            var-map)
          ; otherwise put it in
          (hash-set var-map l r))))
    (ann (hash) (HashTable Symbol Const-Expr))
    ; If left hand side is already an integer, ignore it
    (filter (λ (pair)
               (match-define (cons l r) pair)
               (not (integer? l)))
            (zip lhs rhs))))

; Transform a recurrence relation applied m times into
; an equation. For its single use in fold right now,
; we assume a formula will only be an addition of a
; variable and constant number.
(: derive-recur-eq (-> Const-Expr Const-Expr Const-Expr))
(define (derive-recur-eq rec m)
  (define recf : Const-Expr (normal-form rec))
  ; Because constants come first in canonical form,
  ; we know the constant is the first element in an add
  (match recf
    [`(+ ,k ,n) (normal-form `(* ,k ,m))]
    [_ (context-error "Only addition supported in recurrence relations right now")]))

;; Entry point: transforms a whole program
(: @-transform (-> @-Ast $program))
(define (@-transform ast)
  (match (dectx ast)
    [`(@program ,initial-defs
                ,body)
     ; Generate accessor fns for all struct types defined
     (define accessor-fn-defs
       (flatten1 (map generate-accessors
                      (filter struct-def? initial-defs))))
     (define definitions (definitions-sort
                           (append initial-defs accessor-fn-defs)))
     (define type-scope (definitions->scope definitions))
     (define const-gen-fn-names (map def->name (filter const-generic? definitions)))

     ; Monomorphize const-generic function definitions

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
                                           (map (λ((x : (List Symbol
                                                                Type-Expr))) :
                                                  (Pairof Symbol Type)
                                                  (cons (first x)
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
                           ,const-params
                           ,args-with-types
                           ,return-type
                           ,body)
         (snippet)]
       [`(@def-var ,name ,body)
        (match-define (cons $body _) (@->$ body type-scope))
        (set! $vardefs (cons ($vardef name $body) $vardefs))]
       [_ (void)]))
     (let ([$body (car (@->$ body type-scope))]
           ; monomorphize const-generic function defs
           [monomorphized-defs
            (monomorphize-const-gen-fns $definitions body type-scope)]
           [concrete-$defs
            (filter (λ((def : $fndef)) (not (member ($fndef-name def) const-gen-fn-names))) $definitions)])
       ($program (append concrete-$defs (set->list monomorphized-defs))
                 $vardefs
                 $body))]))

; When given a @def-struct, generates a list of @def-fun's
; to access every field of the struct.
(: generate-accessors (-> Definition (Listof Definition)))
(define (generate-accessors def)
  (match def
    [`(@def-struct ,struct-name ,binds)
     (map (λ ((tuple : (List Integer Symbol Type-Expr)))
            (match-define (list i field texpr) tuple)
            `(@def-fun
              ; TODO mangle
              ,(accessor-name struct-name field)
              ;,(string->symbol (format "~a-~a" struct-name (car field)))
              ((@x (@type-struct ,struct-name ,binds)))
              #f
              ; Add one to index i bcs first is always the struct id
              (@index (@var @x) (@lit-num ,(cast (+ i 1) Nonnegative-Integer)))))
          (enumerate binds))]
    [_ '()]))

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
         "expected type ~a, got type ~a in expression ~a"
         (type->string type)
         (type->string real-type)
         (dectx @-ast)))))
  
  ;; shorthand
  (define $type $-Ast-type)

  (define types-map (Type-Scope-type-vars type-scope))
  (parameterize ([current-context (context-of @-ast)])
    (match (dectx @-ast)
      ;; literals
      [`(@lit-num ,num) (cons ($-Ast (TNatRange num num)
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

                    ($-Ast type
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
      [`(@fold ,expr ,var ,acc-var ,acc-type-expr ,ini-val ,l)
        (letrec
          ([$ini-val (car (@->$ ini-val type-scope))]
           [$l (car (@->$ l type-scope))]
           [l-type ($-Ast-type $l)]
           [acc-type (resolve-type acc-type-expr type-scope)]
           [ts (bind-var type-scope acc-var acc-type)])
         (match l-type
           ; If mapping on a Vectorof, return an $-Ast
           [(TVectorof inner-type count)
            (letrec
                ([inner-ts (bind-var ts var inner-type)]
                 [$expr-incomplete (car (@->$ expr inner-ts))]
                 [$expr-type ($-Ast-type $expr-incomplete)]
                 [final-type
                  (cond
                   [(subtype-of? (TDynVectorof (TAny)) $expr-type)
                    ; check that initial value is also a vector
                    (let ([ini-len (match ($-Ast-type $ini-val)
                                     [(TVectorof it l) l]
                                     [(TVector l) (length l)])])
                      ; add initial length to final
                      (match $expr-type
                        [(TVectorof t step-size)
                         (TVectorof
                           t
                           (derive-recur-eq
                             step-size
                             (normal-form `(+ ,count ,ini-len))))]))]
                    [else $expr-type])]
                 ; redefine $expr to have final-type
                 [$expr ($-Ast final-type ($-Ast-node $expr-incomplete))])
              ; TODO check that initial and final types match if primitive or
              ; are both vectors/bytes
              ; Return an $-Ast
              (cons ($-Ast final-type
                           ($fold $expr var acc-var $ini-val $l))
                    tf-empty))]
           [(TVector il)
            (letrec
                ([count (length il)]
                 [inner-type (tvector-inner-type l-type)]
                 [inner-ts (bind-var ts var inner-type)]
                 [$expr-incomplete (car (@->$ expr inner-ts))]
                 [final-type (match ($-Ast-type $expr-incomplete)
                               [(TUnion TNone (TVectorof t step-size))
                                (TVectorof t (derive-recur-eq step-size count))]
                               [x x])]
                 ; redefine $expr to have final-type
                 [$expr ($-Ast final-type ($-Ast-node $expr-incomplete))])
              ; TODO check that initial and final types match if primitive or
              ; are both vectors/bytes
              ;(match (cons ($-Ast-type $ini-val) final-type)
              ;(match ($-Ast-type $ini-val)
                ;[(TVector 
                ;[(? or TNat TBool (if 
              ; Return an $-Ast
              (cons ($-Ast final-type
                           ($fold $expr var acc-var $ini-val $l))
                    tf-empty))]
           ; Otherwise error
           [(var t) (context-error "fold needs to iterate over a
                                    vector, but a ~a was provided"
                                   (type->string t))]))]
      [`(@for ,expr ,var-name ,vec-expr)
       (letrec
           ([$vec-expr (car (@->$ vec-expr type-scope))]
            ;[$vec-expr (car $vec-pair)]
            [vec-type ($-Ast-type $vec-expr)])
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
         (define y-type ($type $y))
         ;(define generate-list (λ (l n)

         ; vector multiply syntax
         (cons (match (cons $x $y)
                 ; literal numbers
                 [(cons ($-Ast (TVector inner-types) ($lit-vec l)) ($-Ast _ ($lit-num n)))
                  (if (eq? 1 (length l))
                      ($-Ast (TVectorof (car inner-types) n)
                             ($lit-vec (make-list n (car l))))
                      ($-Ast (TVectorof (TVector inner-types) n)
                             ($lit-vec (cast (make-list n l) (Listof $-Ast)))))]
                 [(cons ($-Ast _ ($lit-num n)) ($-Ast (TVector inner-types) ($lit-vec l)))
                  (if (eq? 1 (length l))
                      ($-Ast (TVectorof (car inner-types) n)
                             ($lit-vec (make-list n (car l))))
                      ($-Ast (TVectorof (TVector inner-types) n)
                             ($lit-vec (cast (make-list n l) (Listof $-Ast)))))]
                 [_
                  (assert-type x ($type $x) (TNat))
                  (assert-type y ($type $y) (TNat))
                  ($-Ast (match op
                           ['@+ (match (cons (type->natrange ($type $x))
                                             (type->natrange ($type $y)))
                                  [(cons (TNatRange x-a x-b)
                                         (TNatRange y-a y-b))
                                   (TNatRange (and x-a y-a (ce+ x-a y-a))
                                              (and x-b y-b (ce+ x-b y-b)))]
                                  [_ (TNat)])]
                           [_ (TNat)])
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
      [`(@cons ,x ,y)
       ; no type facts can possibly propagate out of an @append
       (match-let ([(cons $x _) (@->$ x type-scope)]
                   [(cons $y _) (@->$ y type-scope)])
         (cons
          ($-Ast (type-cons ($type $x) ($type $y))
                 ($cons $x $y))
          tf-empty))]
      ; TODO currently not used since a specific "push" syntax is not
      ; conventional. Would be useful to use this as an optimization,
      ; for instance, v ++ [1] becomes a push
      [`(@push ,x ,y)
       ; no type facts can possibly propagate out of an @append
       (match-let ([(cons $x _) (@->$ x type-scope)]
                   [(cons $y _) (@->$ y type-scope)])
         (cons
          ($-Ast (type-push ($type $x) ($type $y))
                 ($push $x $y))
          tf-empty))]
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
       (match-define (cons $val _) (@->$ val type-scope))
       #|
       (: idx Nonnegative-Integer)
       (define idx
         (match (dectx idx-expr)
           [`(@lit-num ,x) x]
           [other (context-error "non-literal index ~a not yet supported" other)]))
       |#
       (cons
         (match (dectx idx-expr)
           [`(@lit-num ,idx)
             ($-Ast (type-index ($type $val) idx)
                    ($index $val ($-Ast (TNat) ($lit-num idx))))]
           ; Doesn't check if index is out of bounds
           [`(@var ,name)
             ($-Ast (vec-index-type ($type $val))
                    ($index $val ($-Ast (TNat) ($var name))))]
           [other (context-error "non-literal index ~a not yet supported" other)])
         tf-empty)]
       #|
       (cons
        ($-Ast (type-index ($type $val) idx)
               ($index $val ($-Ast (TNat) ($lit-num idx))))
        tf-empty)]
      |#
      [`(@range ,from ,to)
       (define len (- to from))
       (cons
        ($-Ast (TVectorof (TNat) len)
               ($range ($-Ast (TNat) ($lit-num from))
                       ($-Ast (TNat) ($lit-num to))))
        tf-empty)]
      [`(@slice ,val ,from-expr ,to-expr)
       ;(: idx Nonnegative-Integer)
       #|
       (define to-idx (λ ((expr : @-Ast))
                        (match (dectx expr)
                          [`(@var ,name)
                            ($-Ast (vec-index-type ($type $val))
                                   ($index $val ($-Ast (TNat) ($var name))))]
                          [`(@lit-num ,x) x]
                          [other (context-error "non-literal index ~a not yet supported"
                                                other)])))
       (define to (to-idx to-expr))
       (define from (to-idx from-expr))
       |#
       (match-define (cons $val _) (@->$ val type-scope))
       #|
       (define inner-type
         (match ($type $val)
           [(TVectorof it _) it]
           [(TVector _) (tvector-inner-type ($type $val))]))
       |#
       #|
       (let ([to (match (dectx to-expr)
                   [`(@var ,name) ($-Ast (TNat) ($var name))]
                   [`(@lit-num ,x) ($-Ast (TNat) ($lit-num x))]
                   [other (context-error "slice index must be a literal or
                                          variable, got an ~a"
                                         other)])]
             [from (match (dectx from-expr)
                     [`(@var ,name) ($-Ast (TNat) ($var name))]
                     [`(@lit-num ,x) ($-Ast (TNat) ($lit-num x))]
                     [other (context-error "slice index must be a literal or
                                            variable, got an ~a" other)])])
         |#
         ; TODO restrict type to just the slice, not the union of the whole
         ; vector
         (cons
          ; TODO wrong type
          ($-Ast ;(TNatRange from-expr to-expr)
                 (type-slice ($type $val) from-expr to-expr)
                 ($slice $val from-expr to-expr))
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

          ; Check that parameter types match definition
          (for ([arg args]
                [$arg $args]
                [arg-type arg-types])
            (assert-type arg ($type $arg) arg-type))

          (cons ($-Ast result
                       ($apply fun $args))
                tf-empty)]
         [_ (error '@-ast->type "[~a] WEIRDLY undefined function ~a"
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

; Generate a TFunction type from
; elements of a deconstructed @def-fun
(: make-fn-type (-> Type-Scope
                    (Listof (List Symbol Type-Expr))
                    Symbol
                    (U False Type-Expr)
                    @-Ast
                    TFunction))
(define (make-fn-type ts params-with-types name return-type body)
  (define inner-type-scope
    (foldl (λ((x : (List Symbol Type-Expr))
              (ts : Type-Scope))
             (bind-var ts (first x)
                       (resolve-type (second x) ts)))
           ts
           params-with-types))
  (match-define (cons $body _) (@->$ body inner-type-scope))
  (define ret-type (if return-type
                       (resolve-type return-type ts)
                       ($-Ast-type $body)))
  (unless (subtype-of? ($-Ast-type $body) ret-type)
    (context-error "function ~a annotated with return type ~a but actually returns ~a"
                   name
                   (type->string ret-type)
                   (type->string ($-Ast-type $body))))
  (TFunction
   (map (λ((x : Type-Expr))
          (resolve-type x ts))
        (map (λ((x : (List Symbol Type-Expr)))
               (second x)) params-with-types))
   ($-Ast-type $body)))

; Create a concrete definition from all @apply's found
; in an @-Ast if possible. Note that to fully monomorphize,
; this will need to be run on each new definition
; produced as well.
(: monomorphize-const-gen-fns
   ;(-> (Listof Definition) @-Ast Type-Scope (Setof Definition)))
   (-> (Listof $fndef) @-Ast Type-Scope (Setof $fndef)))
(define (monomorphize-const-gen-fns defs ast ts)
  ((inst @ast-recurse (Setof $fndef))
    (λ(ast $ N)
      (define $ast (car (@->$ ast ts)))
      (match ($-Ast-node $ast)
        [($apply name args)
          (let ([def (find-$def-by-name name defs)])
            (if def
              (let ([concrete-def (const-generic->concrete def $ast ts)]
                   [args-set
                    (for/fold ([accum : (Setof $fndef) (set)])
                              ([counter (length (ann args (Listof $-Ast)))])
                      (set-union accum ($ counter)))])
                (if concrete-def
                  (set-union (set concrete-def) args-set)
                  args-set))
              (context-error "Missing a function definition for ~a during
                             monomorphization" name)))]
        [_ (for/fold ([accum : (Setof $fndef) (set)])
                     ([counter N])
             (set-union accum ($ counter)))]))
    ast))

; Takes a @def-generic-fun definition and an @apply ast node
; and returns a @def-fun definition where the const-generic types are
; replaced with types inferred by the application; or nothing if inference
; is not possible.
(: const-generic->concrete (-> $fndef $-Ast Type-Scope (Option $fndef)))
(define (const-generic->concrete gen-def app ts)
  ; TODO check that definition and ast fns match
  (match gen-def
    [($fndef def-name binds body)
      (match ($-Ast-node app)
        [($apply name args)
          (letrec ([arg-types (map $-Ast-type args)]
                   [unified ((hash-ref (Type-Scope-funs ts) name) arg-types)]
                   [is-concrete (foldl (λ(t acc) (and (has-concrete-const-expr? t) acc))
                                       #t
                                       (cons (TFunction-result-type unified)
                                             (TFunction-arg-types unified)))]
                   [cg-table
                    (for/fold ([accum : (Immutable-HashTable Symbol Const-Expr) (hash)])
                              ([param-type (map cdr binds)]
                               [arg-type arg-types])
                      (define-values (type-table cg-table) (type-unify param-type arg-type))
                      (hash-union accum cg-table))])
            (if is-concrete
              ; Produce a new definition from the unified type sig
              ($fndef def-name
                      (zip (map car binds)
                           (TFunction-arg-types unified))
                      (foldl (λ(pair body)
                               (match-define (cons sym val) pair)
                               (repl-const-generics body sym val))
                             body
                             (hash->list cg-table)))
              #f))]
        [_ (context-error "Internal error: expected an apply ast node but
                          got ~a" app)])]))



#|
(: def-unify-const-expr (-> Definition @-Ast Type-Scope Definition))
(define (def-unify-const-expr def app ts)
   ; First bind any constant vars to the type scope
   (define tf-with-params (make-fn-type accum binds name return-type body))
   ;; resolve const expressions and do unification
   (bind-fun accum
             name
             (lambda ((callsite-arg-types : (Listof Type)))
                      ;(callsite-vals : (Listof $-Ast-variant)))
               (match tf-with-params
                 [(TFunction param-types res-type)
                  (letrec ([param-exprs
                          ; this map is a typecheck hack
                          (map (λ(x) (cast x (Pairof Integer Const-Expr)))
                               (filter (λ(pair) (match-define (cons i e) pair)
                                                ((compose not false?) e))
                                       (enumerate (map get-const-expr
                                                       param-types))))]
                          [arg-exprs
                          ; this map is a typecheck hack
                          (map (λ(x) (cast x (Pairof Integer Const-Expr)))
                               (filter (λ(pair) (match-define (cons i e) pair)
                                                ((compose not false?) e))
                                       (enumerate (map get-const-expr
                                                       callsite-arg-types))))]
                          [const-var-map
                          ; get a mapping of const vars to their inferred
                          ; const-exprs based on the callsite args
                          (solve-const-exprs (map cdr param-exprs)
                                             const-params
                                             (map cdr arg-exprs))]
                          [substd-param-exprs
                          ; for each param, subst each const var mapping
                          (map (λ(e) (foldl (λ(pair acc)
                                              (match-define (cons var var-expr) pair)
                                              (subst-const-expr acc var var-expr))
                                            e
                                            (hash->list const-var-map)))
                               (map cdr param-exprs))]
                          [arg-types
                          ; Splice the substituted const exprs back into the
                          ; original param-types
                          (let ([arg-types param-types])
                            (foldl
                              (λ(pair types)
                                (match-define (cons expr-idx (cons param-idx-uncasted _)) pair)
                                (define param-idx (cast param-idx-uncasted Integer))
                                (list-set types
                                          param-idx
                                          (repl-const-expr (list-ref types param-idx)
                                                           (list-ref
                                                             substd-param-exprs
                                                             expr-idx))))
                              param-types
                              (enumerate param-exprs)))]
                          ; Replace the const expr in the result type if it exists
                          [result-type
                          (let ([res-expr (get-const-expr res-type)])
                            (if res-expr
                              (repl-const-expr
                                res-type
                                (foldl (λ(pair acc)
                                         (match-define (cons var var-expr) pair)
                                         (subst-const-expr acc var var-expr))
                                       res-expr
                                       (hash->list const-var-map)))
                              res-type))])
                    |#

; takes a Type-Scope rather than just one map because
; types may need to be resolved and the function-scope
; should also be added to.
(: add-fun-def (-> Definition Type-Scope Type-Scope))
(define (add-fun-def def accum)
  (match def
    [`(@def-fun ,name
                ,binds
                ,return-type
                ,body)
     ;; A "noop" generic type
     (define t (make-fn-type accum binds name return-type body))
     (bind-fun accum
               name
               (λ _ t))]
    [`(@def-generic-fun ,name
                        ,generic-params
                        ,const-params
                        ,binds
                        ,return-type
                        ,body)
     ; First bind any constant vars to the type scope
     (define tf-with-params (make-fn-type accum binds name return-type body))
       ;; resolve const expressions and do unification
       (bind-fun accum
                 name
                 (lambda ((callsite-arg-types : (Listof Type)))
                          ;(callsite-vals : (Listof $-Ast-variant)))
                   (match tf-with-params
                     [(TFunction param-types res-type)
                      (define type-unification-table
                        (for/fold ([accum : (Immutable-HashTable TVar Type) (hash)])
                                  ([arg-type param-types]
                                   [callsite-arg-type callsite-arg-types])
                          (define-values (type-table cg-table) (type-unify arg-type callsite-arg-type))
                          (hash-union accum
                                      type-table)))
                      (define cg-unification-table
                        (for/fold ([accum : (Immutable-HashTable Symbol Const-Expr) (hash)])
                                  ([arg-type param-types]
                                   [callsite-arg-type callsite-arg-types])
                          (define-values (type-table cg-table) (type-unify arg-type callsite-arg-type))
                          (hash-union accum
                                      cg-table)))
                      (TFunction (map (λ((x : Type))
                                        (cg-template-fill (type-template-fill x type-unification-table)
                                                          cg-unification-table))
                                      param-types)
                                 (cg-template-fill (type-template-fill res-type type-unification-table)
                                                   cg-unification-table))])))]
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
  (require "../grammar/parser.rkt")
  (parameterize ([FILENAME "test.melo"])
    (time
     (type->string 
      ($-Ast-type
       ($program-expr
        (@-transform
         (time
          (melo-parse-port (open-input-string "
def laboo<const N, T>(x: [T; N]) = x ++ x
---

laboo([1, 2, 3])
"))))))))))