#lang typed/racket
(require "types.rkt")
;; untyped internal impl
(module untyped racket
  (require "lexer.rkt")
  (require (prefix-in T "types.rkt"))
  (require parser-tools/lex)
  (require parser-tools/cfg-parser)
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
                    ((DEF FUN OPEN-PAREN <fun-args> CLOSE-PAREN = <expr>)
                     `(@def-fun ,$2 ,$4 #f ,$7))
                    ((DEF FUN OPEN-PAREN <fun-args> CLOSE-PAREN ARROW <type-expr> = <expr>)
                     `(@def-fun ,$2 ,$4 ,$7 ,$9))
                    )
      (<fun-args> ((<type-dec>) (list $1))
                  ((<type-dec> <fun-args>) (cons $1 $2)))
      (<type-dec> ((VAR COLON <type-expr>) (list $1 $3)))
      (<type-expr> ((TYPE) `(@type-var ,$1))
                   ((OPEN-BRACKET <type-exprs> CLOSE-BRACKET) `(@type-vec ,$2)))
      (<type-exprs> ((<type-expr>) (list $1))
                    ((<type-expr> <type-exprs>) (cons $1 $2)))
      ;; different kinds of exprs
      (<expr> ((<add-expr>) $1)
              ((<let-expr>) $1)
              ((<block-expr>) $1)
              )
      ;; function application
      (<apply-expr> ((FUN OPEN-PAREN <multi-exprs> CLOSE-PAREN) (pos-lift 1 4 `(@apply ,$1 ,$3))))
      ;; vectors
      (<vector-expr> ((OPEN-BRACKET <multi-exprs> CLOSE-BRACKET) (pos-lift 1 3 `(@lit-vec ,$2))))
      ;; blocks
      (<block-expr> ((BEGIN <multi-exprs> END) (pos-lift 1 3 `(@block ,$2))))
      (<multi-exprs> ((<expr>) (list $1))
                     ((<expr> <multi-exprs>) (cons $1 $2)))
      ;; let x = y in expr
      (<let-expr> ((LET VAR = <expr> IN <expr>) (pos-lift 1 6
                                                          `(@let (,$2 ,$4) ,$6))))

      
      ;; low-associativity (add-like) operators
      (<add-expr> ((<add-expr> + <mult-expr>) (pos-lift 1 3 `(@+ ,$1 ,$3)))
                   ((<add-expr> - <mult-expr>) (pos-lift 1 3 `(@- ,$1 ,$3)))
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
                       ((<apply-expr>) $1))
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
               [context->string (-> (Option context) String)]
               [FILENAME (Parameter String)]
               )
(provide melo-parse-port
         context->string
         FILENAME)

(module+ main
  (pretty-display
   (dectx*
    (melo-parse-port (open-input-string "
def @f (x:Nat y:Nat) = x +y
def @g (x:Nat y:Nat) -> [Nat Nat] = [x*2 y*3]

x + @g (y)
")))))