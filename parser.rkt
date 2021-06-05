#lang typed/racket
(require "common.rkt")
;; untyped internal impl
(module untyped racket
  (require "lexer.rkt")
  (require (prefix-in T "common.rkt"))
  (require parser-tools/lex)
  (require parser-tools/cfg-parser)
  (require parser-tools/yacc)
  (require compatibility/defmacro)
  ;; we only provide melo-parse-port because it has a much saner API
  (provide melo-parse-port
           FILENAME)
  
  ;; melo-parse-port: Input-Port -> @-Ast
  (define (melo-parse-port x)
    (port-count-lines! x)
    (time (melo-parse (lambda () (melo-lex-once x)))))
  
  ;; melo-parse: (-> position-token) -> @-Ast
  (define melo-parse
    (parser
     (start <program>)
     (end EOF)
     (tokens value-tokens syntax-tokens)
     
     (src-pos)
     ;(debug "/tmp/out.txt")
     ;(yacc-output "/tmp/debug.y")
     (error (lambda (tok-ok? tok-name tok-value start-pos end-pos)
              (raise-syntax-error
               'melo-parse
               (if tok-value (format "Unexpected ~a \"~a\"" tok-name tok-value)
                   (format "Unexpected ~a" tok-name))
               (datum->syntax
                #f
                (FILENAME)
                (vector
                 (FILENAME)
                 (position-line start-pos)
                 (position-col start-pos)
                 (position-offset start-pos)
                 (- (position-offset end-pos)
                    (position-offset start-pos)))))))
     
     ;; CFG grammar
     (grammar
      ;; a program is a series of toplevels 
      (<program> ((<definitions> <expr>) `(@program ,$1 ,$2))
                 ((<expr>) `(@program () ,$1)))
      (<definitions> ((<definition> <definitions>) (cons $1 $2))
                     ((<definition>) (list $1)))
      (<definition> ((DEF VAR = <expr>) `(@def-var ,$2 ,$4))
                    ((DEF VAR OPEN-PAREN <fun-args> CLOSE-PAREN = <expr>)
                     `(@def-fun ,$2 ,$4 #f ,$7))
                    ((DEF VAR OPEN-PAREN <fun-args> CLOSE-PAREN <type-expr> = <expr>)
                     `(@def-fun ,$2 ,$4 ,$6 ,$8))
                    )
      (<fun-args> ((<type-dec>) (list $1))
                  ((<type-dec> COMMA <fun-args>) (cons $1 $3)))
      (<type-dec> ((VAR <type-expr>) (list $1 $2)))
      (<type-expr> ((TYPE) `(@type-var ,$1))
                   ((OPEN-BRACKET <type-exprs> CLOSE-BRACKET) `(@type-vec ,$2))
                   ((OPEN-BRACKET <type-expr> NUM CLOSE-BRACKET) `(@type-vecof ,$2 ,$3)))
      (<type-exprs> ((<type-expr>) (list $1))
                    ((<type-expr> COMMA <type-exprs>) (cons $1 $3)))
      ;; different kinds of exprs
      (<expr> ((<add-expr>) $1)
              ((<let-expr>) $1)
              ((<where-expr>) $1)
              )
      ;; function application
      (<apply-expr> ((VAR OPEN-PAREN <multi-exprs> CLOSE-PAREN) (pos-lift 1 4 `(@apply ,$1 ,$3))))
      ;; vectors
      (<vector-expr> ((OPEN-BRACKET <multi-exprs> CLOSE-BRACKET) (pos-lift 1 3 `(@lit-vec ,$2))))
      (<multi-exprs> ((<expr>) (list $1))
                     ((<expr> COMMA <multi-exprs>) (cons $1 $3)))
      ;; let x = y in expr
      (<let-expr> ((LET VAR = <add-expr> IN <expr>) (pos-lift 1 6
                                                              `(@let (,$2 ,$4) ,$6))))
      ;; expr where x = y
      (<where-expr> ((<add-expr> WHERE VAR = <add-expr>) (pos-lift 1 5
                                                                   `(@let (,$3 ,$5) ,$1)))
                    ((<where-expr> WHERE VAR = <add-expr>) (pos-lift 1 5
                                                                   `(@let (,$3 ,$5) ,$1))))
      
      
      ;; low-associativity (add-like) operators
      (<add-expr> ((<add-expr> + <mult-expr>) (pos-lift 1 3 `(@+ ,$1 ,$3)))
                  ((<add-expr> - <mult-expr>) (pos-lift 1 3 `(@- ,$1 ,$3)))
                  ((<add-expr> ++ <mult-expr>) (pos-lift 1 3 `(@append ,$1 ,$3)))
                  ((<mult-expr>) $1))
      ;; high-associativity (mult-like) binary operators
      (<mult-expr> ((<mult-expr> * <terminal-expr>) (pos-lift 1 3 `(@* ,$1 ,$3)))
                   ((<mult-expr> / <terminal-expr>) (pos-lift 1 3 `(@/ ,$1 ,$3)))
                   ((<terminal-expr>) $1))
      ;; terminal expressions
      (<terminal-expr> ((NUM) (pos-lift 1 1 `(@lit-num ,$1)))
                       ((VAR) (pos-lift 1 1 `(@var ,$1)))
                       ((OPEN-PAREN <expr> CLOSE-PAREN) (pos-lift 1 3 $2))
                       ((<vector-expr>) $1)
                       ((<apply-expr>) $1)
                       )
      )))
  
  
  ;; [[ utility functions for tracking positions ]]
  
  ;; parameter that maybe contains a filename
  (define FILENAME (make-parameter "<<unknown file>>"))
  
  (define-macro (pos-lift a b expr)
    `(let ([poz-a ,(string->symbol (format "$~a-start-pos" a))]
           [poz-b ,(string->symbol (format "$~a-end-pos" b))])
       (Twith-context (Tcontext (FILENAME)
                                poz-a
                                poz-b)
                      ,expr))))
;; typed interface
(require/typed 'untyped
               [melo-parse-port (-> Input-Port @-Ast)]
               [FILENAME (Parameter String)]
               )
(provide melo-parse-port
         context->string
         FILENAME)

(module+ main
  (pretty-display
   (dectx*
    (melo-parse-port (open-input-string "
(x where x = x) where x = 1
")))))