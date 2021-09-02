#lang typed/racket
(require "raw-ast.rkt"
         "../type-sys/types.rkt")
(require typed-map)
(provide add-parents)

; Returns any parent names found strictly in the
; top-level bindings of a definition. Does not
; inspect the sub @-Ast nodes.
(: add-parents-in-def
   (-> Definition
       (Pairof (Listof Symbol) (Listof Symbol))
       (Pairof (Listof Symbol) (Listof Symbol))))
(define (add-parents-in-def def acc)
  (match-define (cons parents blacklist) acc)

  (: add-n (-> Symbol (Listof Symbol)))
  (define (add-n name)
        (if (not (member name blacklist))
          (cons name parents)
          parents))

  (match def
    [`(@def-fun ,name ,params ,_ ,_)
      (cons
        ; TODO check param type exprs for parents
        parents
        (append (map car params) blacklist))]
    [`(@def-generic-fun ,name ,_ ,params ,_ ,_)
      (cons
        ; TODO check param type exprs for parents
        parents
        (append (map car params) blacklist))]
    [_ acc]))

(: add-parents
   (-> @-Ast
       (Pairof (Listof Symbol) (Listof Symbol))
       (Pairof (Listof Symbol) (Listof Symbol))))
(define (add-parents ast acc)
  (match-define (cons parents blacklist) acc)

  (: add-n (-> Symbol (Listof Symbol)))
  (define (add-n name)
        (if (not (member name blacklist))
          (cons name parents)
          parents))

  (match (dectx ast)
    [`(@program ,defs ,_)
      (foldl add-parents-in-def acc defs)]
    [`(@let (,var ,_) ,_)
      (cons parents (cons var blacklist))]
    [`(@apply ,name ,_)
      (cons (add-n name) blacklist)]
    [`(@instantiate ,name ,_)
      (cons (add-n name) blacklist)]
    [`(@var ,name)
      (cons (add-n name) blacklist)]
    [_ acc]))

;(: def-parents (-> Definition (Set Symbol)))
;(define (def-parents def)
