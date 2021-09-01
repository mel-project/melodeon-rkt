#lang typed/racket
(require "raw-ast.rkt"
         "../type-sys/types.rkt")
(provide add-parents)

(: add-parents (-> @-Ast
                   (Pairof (Listof Symbol) (Listof Symbol))
                   (Pairof (Listof Symbol) (Listof Symbol))))
(define (add-parents ast acc)
  (match-define (cons parents blacklist) acc)
  (define (name-ok name) (not (member name blacklist)))

  (match ast
    [`(@let (,var ,val) ,expr)
      (cons parents (cons var blacklist))]
    [`(@apply ,name ,_)
      (cons
        (if (name-ok name)
          (cons name parents)
          parents)
        blacklist)]
    [_ (cons parents blacklist)]))

;(: def-parents (-> Definition (Set Symbol)))
;(define (def-parents def)
