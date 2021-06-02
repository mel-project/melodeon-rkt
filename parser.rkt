#lang typed/racket
(require typed/racket/unsafe)
(require "types.rkt")
;; untyped internal impl
(module untyped racket
  (require "lexer.rkt")
  (require (prefix-in T "types.rkt"))
  (require parser-tools/lex)
  (require parser-tools/yacc)
  (require compatibility/defmacro)
  ;; we only provide melo-parse-port because it has a much saner API
  (provide melo-parse-port
           context->string
           FILENAME)
  
  
  (define (context->string ctx)
    (if ctx
        (format "~a, ~a:~a-~a:~a"
                (Tcontext-filename ctx)
                (position-line (Tcontext-start-pos ctx))
                (position-col (Tcontext-start-pos ctx))
                (position-line (Tcontext-end-pos ctx))
                (position-col (Tcontext-end-pos ctx)))
        "NO CONTEXT"))
  
  ;; melo-parse-port: Input-Port -> @-Ast
  (define (melo-parse-port x)
    (port-count-lines! x)
    (melo-parse (lambda () (melo-lex-once x))))
  
  ;; melo-parse: (-> position-token) -> @-Ast
  (define melo-parse
    (parser
     (start <program>)
     (end EOF)
     (tokens value-tokens syntax-tokens)
     
     (src-pos)
     (error (lambda (tok-ok? tok-name tok-value start-pos end-pos)
              (raise-syntax-error
               'mir-parse
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
      ;; a program is a single expression right now
      (<program> ((<expr>) (pos-lift 1 1 $1)))
      ;; different kinds of exprs
      (<expr> ((<add-bexpr>) (pos-lift 1 1 $1))
              ((<let-expr>) (pos-lift 1 1 $1))
              ((<vector-expr>) (pos-lift 1 1 $1))
              )
      ;; vectors
      (<vector-expr> ((OPEN-BRACKET <comma-exprs> CLOSE-BRACKET) (pos-lift 1 3 `(@lit-vec ,$2))))
      (<comma-exprs> ((<expr>) (pos-lift 1 1 (list $1)))
                     ((<expr> COMMA <comma-exprs>) (pos-lift 1 3 (cons $1 $3))))
      ;; let x = y in expr
      (<let-expr> ((LET VAR = <expr> IN <expr>) (pos-lift 1 6
                                                          `(@let (,$2 ,$4) ,$6))))
      ;; low-associativity (add-like) binary operators
      (<add-bexpr> ((<add-bexpr> + <mult-bexpr>) (pos-lift 1 3 `(@+ ,$1 ,$3)))
                   ((<add-bexpr> - <mult-bexpr>) (pos-lift 1 3 `(@- ,$1 ,$3)))
                   ((<mult-bexpr>) $1))
      ;; high-associativity (mult-like) binary operators
      (<mult-bexpr> ((<mult-bexpr> * <terminal-expr>) (pos-lift 1 3 `(@* ,$1 ,$3)))
                    ((<mult-bexpr> / <terminal-expr>) (pos-lift 1 3 `(@/ ,$1 ,$3)))
                    ((<terminal-expr>) $1))
      ;; terminal expressions
      (<terminal-expr> ((NUM) (pos-lift 1 1 `(@lit-num ,$1)))
                       ((VAR) (pos-lift 1 1 `(@var ,$1)))
                       ((OPEN-PAREN <expr> CLOSE-PAREN) (pos-lift 1 3 $2)))
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
               [context->string (-> Any String)]
               [FILENAME (Parameter String)]
               )
(provide melo-parse-port
         context->string
         FILENAME)

(module+ main
  (pretty-print
   (melo-parse-port (open-input-string "
((((1 + 2 + 3))))
")))) 