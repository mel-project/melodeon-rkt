#lang typed/racket
(require "parser.rkt"
         "typecheck.rkt"
         "types.rkt")


(: generate-mil (-> @-Ast Mil))
(define (generate-mil @-ast)
  (: strip-@ (-> Symbol Symbol))
  (define (strip-@ @-sym)
    (string->symbol (substring (symbol->string @-sym) 1)))

  (match (dectx @-ast)
    ;; let bindings 
    [`(@let (,var-name ,var-value) ,body)
     `(let (,var-name ,(generate-mil var-value)) ,(generate-mil body))]
    ;; binary ops
    [`(,(? (lambda(op) (member op '(@+ @- @* @/))) op) ,x ,y) (cast `(,(strip-@ op) ,(generate-mil x)
                                                                                    ,(generate-mil y))
                                                                    Mil)]
    [`(@var ,(? symbol? varname)) varname]
    [`(@lit-num ,(? exact-integer? number)) number]
    [`(@lit-vec ,vv) (list->vector (map generate-mil vv))]
    [other (error "invalid @-ast" other)]))


;; demo compiler flow
(module+ main
  (define ast (parameterize ([FILENAME "whatever.melo"])
                (melo-parse-port (open-input-string "
let x = 123 in
let y = 456 in
    x + (let y = y*y*123545 in y * x + y+(((((((((((((y))))))))))))))
"))))
  (displayln "@-Ast:")
  (pretty-display ast)
  (pretty-print (get-context ast))
  (displayln "")
  ;; type check
  (void (@-ast->type ast (hash)))
  ;; generate the mil
  (displayln "Mil:")
  (pretty-display
   (generate-mil ast)))