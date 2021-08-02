#lang typed/racket
(require "types.rkt"
         "resolver.rkt"
         "../common.rkt")
(provide full-unify
         type-index)

(module+ test
  (define T (TVar 'T))
  (define U (TVar 'U))
  (define template (TVectorof T 3))
  (define type (TIntersect (TIntersect (TUnion (TVector (list (TNat) (TAny) (TNat))) (TNat))
                                       (TUnion (TNegate (TNat)) (TVectorof (TNat) 3)))
                           (TVectorof (TNat) 3)))
  (match-define (list bindings unified) (full-unify template type))
  (printf "template: ~a\n\n" (type->string template))
  (printf "unifying: ~a\n\n" (type->string type))
  (printf "'T = ~a\n\n" (type->string (hash-ref bindings (TVar 'T))))
  (printf "result: ~a\n\n"
             (type->string unified))
  (time (printf "valid?: ~a\n" (subtype-of? unified (TAny))))
  )

;; *** NOTE ***
;;
;; Unification can be done recursively! If we are unifying T | U with V, we can unify T with V, U with V, and pick whichever works. If we're unifying T & U with V, we unify T with V, U with V, and intersect them.

(: find-tvar (-> Type (Option TVar)))
(define (find-tvar type)
  (match type
    [(TVar _) type]
    [(TVectorof t _) (find-tvar t)]
    [(TVector tt) (ormap find-tvar tt)]
    [(TUnion t u) (or (find-tvar t)
                      (find-tvar u))]
    [(TIntersect t u) (or (find-tvar t)
                          (find-tvar u))]
    [(TNegate t) (find-tvar t)]
    [_ #f]))

(: replace-tvar (-> Type TVar Type Type))
(define (replace-tvar type tvar replacement)
  (: recurse (-> Type Type))
  (define (recurse type) (replace-tvar type tvar replacement))
  (cond
    [(equal? tvar type) replacement]
    [else 
     (match type
       [(TVectorof t n) (TVectorof (recurse t) n)]
       [(TVector tt) (TVector (map recurse tt))]
       [(TUnion t u) (TUnion (recurse t)
                             (recurse u))]
       [(TIntersect t u) (TIntersect (recurse t)
                                     (recurse u))]
       [(TNegate t) (TNegate (recurse t))]
       [x x])]))

;; Full unification: fills in all the holes completely
(: full-unify (-> Type Type (List (HashTable TVar Type) Type)))
(define (full-unify template type)
  (match (find-tvar template)
    [#f (list (hash) template)]
    [(? TVar? first-tvar)
     (define tvar-binding (raw-unify template type first-tvar))
     (cond
       [tvar-binding
        (define tvar-replaced (replace-tvar template first-tvar tvar-binding))
        (match-define (list bindings fully-unified) (full-unify tvar-replaced type))
        (list (hash-set bindings first-tvar tvar-binding)
              fully-unified)]
       [else (context-error "unification stuck")])]))

;; Raw unification: unifies LHS "template" with RHS fully qualified type. LHS must have ONLY ONE type variable!
(: raw-unify (-> Type Type TVar (Option Type)))
(define (raw-unify template
                         type
                         variable)
  (match type
    #;[(TUnion t u) (TUnion (raw-unification template t variable)
                            (raw-unification template u variable))]
    #;[(TIntersect t u) (TIntersect (raw-unification template t variable)
                                    (raw-unification template u variable))]
    [type
     ;; type is "fully simplified" now
     (match template
       ; THIS IS DA BASE CASE!
       [(TVar (? (λ((s : Symbol)) (eq? s (TVar-label variable))))) type]
       ; If the template is a union, TODO we pick one that works
       [(TUnion t u) (or (raw-unify t type variable)
                         (raw-unify u type variable))]
       ; TODO this is bad
       [(TIntersect t u) (or
                          (raw-unify t type variable)
                          (raw-unify u type variable))]
       ; If the template is a not, we pass the buck.
       [(TNegate t) (raw-unify t (TNegate type) variable)]
       ; If the template is a vector, we go through the type index-wise, using the tindex function
       [(TVectorof t n) (raw-unify (TVector (make-list n t)) type variable)]
       [(TVector tt) (foldl
                      (λ((t : (Option Type))
                         (accum : Type))
                        (if t (TUnion t accum) accum))
                      (TNone)
                      (for/list ([subtemp tt]
                                 [i (in-naturals)]) : (Listof (Option Type))
                        (raw-unify subtemp
                                   (type-index type i)
                                   variable)))]
       [_ #f])]))

;; Given a type and a type->type function, creates another type, made through invoking the given function only with non-logical types. This is used for generic type inference, vector indexing, etc.
(: type-inner-map (-> Type (-> Type Type) Type))
(define (type-inner-map type type-deconstruct)
  (: clause-map (-> (Setof Type) Type))
  (define (clause-map clause)
    (foldl (λ((type : Type)
              (accum : Type))
             (TIntersect (type-deconstruct type) accum))
           (TAny)
           (set->list clause)))
  ;; break down into dnf
  (define dnf (type->dnf type))
  (displayln (dnf->string dnf))
  (foldl (λ((clause : (Setof Type))
            (t : Type))
           (TUnion (clause-map clause)
                   t))
         (TNone)
         (set->list dnf)))

;; Given a type and an index, gets the output type
(: type-index (-> Type Integer Type))
(define (type-index vector-type idx)
  (: inner (-> Type Type))
  (define (inner vector-type)
    (match vector-type
      [(TVector lst) (if (>= idx (length lst))
                         (gen-tfail vector-type)
                         (list-ref lst idx))]
      [(TVectorof t n) (if (>= idx n)
                           (gen-tfail vector-type)
                           t)]
      [(TBytes n) (if (>= idx n)
                      (gen-tfail vector-type)
                      (TNat))]
      ;; set theory types
      [(TNegate t) (TNegate (inner t))]
      [(TUnion t u) (TUnion (inner t)
                            (inner u))]
      [(TIntersect t u) (TIntersect (inner t)
                                    (inner u))]
      ;; unindexable types
      [(TNone) (TNone)]
      [(TNat) (TUnion (gen-tfail (TBin))
                      (TFail 'baba))]
      [_ (gen-tfail vector-type)]))
  ;; check that it's not a fail type
  (let ([res (inner vector-type)])
    (if (subtype-of? res (TAny))
        res
        (context-error "cannot index into position ~a of ~a"
                       idx
                       (type->string vector-type)))))