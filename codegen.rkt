#lang typed/racket
(require "parser.rkt"
         "typecheck.rkt"
         "types.rkt")


(: generate-mil (-> @-Ast Any))
(define (generate-mil @-ast)
  (: strip-@ (-> Symbol Symbol))
  (define (strip-@ @-sym)
    (string->symbol (substring (symbol->string @-sym) 1)))

  (match (dectx @-ast)
    ;; let bindings
    [`(@program ,definitions ,body)
     (append (map generate-mil-defs definitions)
             (list (generate-mil body)))]
    [`(@let (,var-name ,var-value) ,body)
     `(let (,var-name ,(generate-mil var-value)) ,(generate-mil body))]
    ;; binary ops
    [`(,(? (lambda(op) (member op '(@+ @- @* @/))) op) ,x ,y) `(,(strip-@ op) ,(generate-mil x)
                                                                              ,(generate-mil y))]
    [`(@var ,(? symbol? varname)) varname]
    [`(@lit-num ,(? exact-integer? number)) number]
    [`(@lit-vec ,vv) (list->vector (map generate-mil vv))]
    [`(@apply ,fun ,args) `(,fun . ,(map generate-mil args))]
    [other (error "invalid @-ast" other)]))

(: generate-mil-defs (-> Definition Any))
(define (generate-mil-defs def)
  (match def
    [`(@def-var ,var ,expr) `(gl ,var ,(generate-mil expr))]
    [`(@def-fun ,var ,args ,_ ,expr)
     `(fn ,var ,(map (inst car Symbol Any) args) ,(generate-mil expr))]))


;; demo compiler flow
(module+ main
  (define ast (parameterize ([FILENAME "whatever.melo"])
                (melo-parse-port (open-input-string "
def @f (x:Nat y:Nat) =
  [x+y y*x]
def @g (x:Nat) =
  [@f(x x) x @f(x x)]
def @v (x:[Nat Nat]) -> Nat = 3

@v(@f(1 2))
"))))
  (displayln "@-Ast:")
  (pretty-display (dectx* ast))
  (displayln "")
  ;; type check
  (void (@-ast->type ast (hash)))
  ;; generate the mil
  (displayln "Mil:")
  (pretty-display
   (generate-mil ast)))