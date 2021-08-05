#lang typed/racket
(require "type-bag.rkt"
         "types.rkt"
         "resolver.rkt"
         "../common.rkt")
(provide type-index)

(: type-index (-> Type Integer Type))
(define (type-index type idx)
  (define bagged (type->bag type))
  ;; we first make sure the vector has the right length, because projection is infallible --- it returns a set of facts, not a type
  (for ([length-case (Type-Bag-inner (bag-project bagged `(len root)))])
    (define length (with-handlers ([exn:fail? (λ _ (context-error "type ~a has unknown length"
                                                                  (type->string type)))])
                     (cast (hash-ref length-case 'root) Integer)))
    (unless (< idx length)
      (context-error "cannot index into type ~a with index ~a because it may be of a shorter length ~a"
                     (type->string type)
                     idx
                     length)))
  ;; then we find the possible types
  (bag->type (bag-project bagged `(ref root ,idx))))