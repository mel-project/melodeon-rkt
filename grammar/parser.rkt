#lang typed/racket
(require "../asts/raw-ast.rkt")
;; untyped internal impl
(module untyped racket
  (require "lexer.rkt")
  (require (prefix-in T "../asts/raw-ast.rkt"))
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
    (melo-parse (lambda () (melo-lex-once x))))
  
  ;; melo-parse: (-> position-token) -> @-Ast
  (define melo-parse
    (cfg-parser ; distinguishing type-expr from expr takes unlimited lookahead due to the shared | and & operators,m
                ; so we use cfg-parser rather than parser (which complains of shift/reduce conflicts)
     (start <program>)
     (end EOF)
     (tokens value-tokens syntax-tokens)
     
     (src-pos)
     ; (debug "/tmp/out.txt")
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
      (<program> ((<definitions> --- <expr>) (pos-lift 1 3 `(@program ,$1 ,$3)))
                 ((<definitions>) (pos-lift 1 1 `(@program ,$1 (@lit-num 0))))
                 ((<expr>) `(@program () ,$1)))
      (<definitions> ((<definition> <definitions>) (cons $1 $2))
                     ((<definition>) (list $1)))
      (<definition> ((DEF VAR = <expr>) `(@def-var ,$2 ,$4))
                    ((DEF VAR OPEN-PAREN <fun-args> CLOSE-PAREN = <expr>)
                     `(@def-fun ,$2 ,$4 #f ,$7))
                    ((DEF VAR OPEN-PAREN <fun-args> CLOSE-PAREN COLON <type-expr> = <expr>)
                     `(@def-fun ,$2 ,$4 ,$7 ,$9))
                    ((DEF VAR LESS-THAN <type-params> GREATER-THAN OPEN-PAREN <fun-args> CLOSE-PAREN = <expr>)
                     `(@def-generic-fun ,$2 ,$4 ,$7 #f ,$10))
                    ((DEF VAR LESS-THAN <type-params> GREATER-THAN OPEN-PAREN <fun-args> CLOSE-PAREN COLON <type-expr> = <expr>)
                     `(@def-generic-fun ,$2 ,$4 ,$7 ,$10 ,$12))
                    ((REQUIRE BYTES) `(@require ,(bytes->string/utf-8 $2)))
                    ((PROVIDE VAR) `(@provide ,$2))
                    ((PROVIDE TYPE) `(@provide ,$2))
                    ((STRUCT TYPE OPEN-BRACE <fun-args> CLOSE-BRACE)
                     `(@def-struct ,$2 ,$4))
                    ((ALIAS TYPE = <type-expr>)
                     `(@def-alias ,$2 ,$4))
                    )
      (<type-params> ((TYPE) (list $1))
                     ((TYPE COMMA <type-params>) (cons $1 $3)))
      (<fun-args> ((<type-dec>) (list $1))
                  ((<type-dec> COMMA <fun-args>) (cons $1 $3))
                  (() empty))
      (<type-dec> ((VAR COLON <type-expr>) (list $1 $3)))
      (<type-expr> ((<type-expr> BOR <type-expr-2>) `(@type-union ,$1 ,$3))
                   ((<type-expr-2>) $1))
      (<type-expr-2> ((<type-expr-3> BAND <type-expr-2>) `(@type-intersect ,$1 ,$3))
                     ((<type-expr-3>) $1))
      (<type-expr-3> ((TYPE) `(@type-var ,$1))
                     ((OPEN-BRACKET <type-expr> * CLOSE-BRACKET) `(@type-dynvecof ,$2))
                     ((PERCENT OPEN-BRACKET NUM CLOSE-BRACKET) `(@type-bytes ,$3))
                     ((PERCENT OPEN-BRACKET CLOSE-BRACKET) `(@type-dynbytes))
                     ((OPEN-BRACKET <type-exprs> CLOSE-BRACKET) `(@type-vec ,$2))
                     ((OPEN-BRACKET <type-expr> * NUM CLOSE-BRACKET) `(@type-vecof ,$2 ,$4))
                     ((OPEN-PAREN <type-expr> CLOSE-PAREN) `$2))
      (<type-exprs> ((<type-expr>) (list $1))
                    ((<type-expr> COMMA <type-exprs>) (cons $1 $3)))
      ;; different kinds of exprs
      (<expr> ((<shl-expr>) $1)
              ((<let-expr>) $1)
              ((<fold-expr>) $1)
              ((<vector-compreh>) $1)
              ((<where-expr>) $1)
              ((<block-expr>) $1)
              ((<set!-expr>) $1)
              )
      ;; set! var = expr
      (<set!-expr> ((SET! VAR = <expr>) (pos-lift 1 4 `(@set! ,$2 ,$4))))
      ;; begin expr expr end
      (<block-expr> ((DO <block-inner> DONE) (pos-lift 1 3 `(@block ,$2))))
      (<block-inner> ((<expr> SEMICOLON <block-inner>) (cons $1 $3))
                     ((<expr>) (list $1)))
      ;; vectors
      (<vector-expr> ((OPEN-BRACKET <multi-exprs> CLOSE-BRACKET) (pos-lift 1 3 `(@lit-vec ,$2))))
      (<multi-exprs> ((<expr>) (list $1))
                     ((<expr> COMMA <multi-exprs>) (cons $1 $3))
                     (() empty))
      ; Vector comprehension
      (<vector-compreh> ((OPEN-BRACKET <expr> FOR VAR IN <expr> CLOSE-BRACKET)
                         (pos-lift 1 3 `(@for ,$2 ,$4 ,$6))))
      ; fold syntax
      (<fold-expr> ((FOLD <expr> FOR VAR VAR FROM <expr> IN <expr>)
                   (pos-lift 1 8 `(@fold ,$2 ,$4 ,$5 ,$7 ,$9))))
      ;; let x = y in expr
      (<let-expr> ((LET VAR = <expr> IN <expr>) (pos-lift 1 6
                                                          `(@let (,$2 ,$4) ,$6)))
                  ((LOOP NUM IN <expr>) (pos-lift 1 4
                                              `(@loop ,$2 ,$4)))
                  ((IF <expr> THEN <expr> ELSE <expr>) (pos-lift 1 6
                                                                 `(@if ,$2 ,$4 ,$6))))
      ;; expr where x = y
      (<where-expr> ((<add-expr> WHERE VAR = <add-expr>) (pos-lift 1 5
                                                                   `(@let (,$3 ,$5) ,$1)))
                    ((<where-expr> WHERE VAR = <add-expr>) (pos-lift 1 5
                                                                     `(@let (,$3 ,$5) ,$1))))
      
      ;; bitwise operators
      (<shl-expr> ((<shl-expr> SHL <shr-expr>) (pos-lift 1 3 `(@shl ,$1 ,$3)))
                 ((<shr-expr>) $1))
      (<shr-expr> ((<shr-expr> SHR <xor-expr>) (pos-lift 1 3 `(@shr ,$1 ,$3)))
                 ((<xor-expr>) $1))
      (<xor-expr> ((<xor-expr> XOR <bor-expr>) (pos-lift 1 3 `(@xor ,$1 ,$3)))
                 ((<bor-expr>) $1))
      (<bor-expr> ((<bor-expr> BOR <band-expr>) (pos-lift 1 3 `(@bor ,$1 ,$3)))
                 ((<band-expr>) $1))
      (<band-expr> ((<band-expr> BAND <or-expr>) (pos-lift 1 3 `(@band ,$1 ,$3)))
                 ((<or-expr>) $1))
      ;; lowest-associativity (logical) operators
      (<or-expr> ((<or-expr> OR <and-expr>) (pos-lift 1 3 `(@or ,$1 ,$3)))
                 ((<and-expr>) $1))
      (<and-expr> ((<and-expr> AND <eq-expr>) (pos-lift 1 3 `(@and ,$1 ,$3)))
                  ((<eq-expr>) $1))
      (<eq-expr> ((<eq-expr> == <add-expr>) (pos-lift 1 3 `(@eq ,$1 ,$3)))
                 ((<add-expr>) $1))
      ;; low-associativity (add-like) operators
      (<add-expr> ((<add-expr> + <mult-expr>) (pos-lift 1 3 `(@+ ,$1 ,$3)))
                  ((<add-expr> - <mult-expr>) (pos-lift 1 3 `(@- ,$1 ,$3)))
                  ((<add-expr> ++ <mult-expr>) (pos-lift 1 3 `(@append ,$1 ,$3)))
                  ((<mult-expr>) $1))
      ;; high-associativity (mult-like) binary operators
      (<mult-expr> ((<mult-expr> * <terminal-expr>) (pos-lift 1 3 `(@* ,$1 ,$3)))
                   ((<mult-expr> / <terminal-expr>) (pos-lift 1 3 `(@/ ,$1 ,$3)))
                   ((<apply-expr>) $1))
      ;; higher-associativity (apply-like) operators
      (<apply-expr> ((VAR OPEN-PAREN <multi-exprs> CLOSE-PAREN) (pos-lift 1 4 `(@apply ,$1 ,$3)))
                    ;; unsafe extern call "f" (args...)
                    ((UNSAFE EXTERN CALL BYTES OPEN-PAREN <multi-exprs> CLOSE-PAREN)
                     (pos-lift 1 7 `(@extern-call ,(bytes->string/utf-8 $4)
                                                  ,$6)))
                    ;; data field accessors
                    ((<apply-expr> DOT VAR) (pos-lift 1 3 `(@accessor ,$1 ,$3)))
                    ;; vector indexing
                    ((<apply-expr> OPEN-BRACKET <terminal-expr> CLOSE-BRACKET) (pos-lift 1 4 `(@index ,$1 ,$3)))
                    ;; vector slice
                    ((<apply-expr> OPEN-BRACKET <terminal-expr> RANGE <terminal-expr> CLOSE-BRACKET)
                     (pos-lift 1 4 `(@range ,$1 ,$3 ,$5)))
                    ;; vector update
                    ((<apply-expr> OPEN-BRACKET <terminal-expr> FAT-ARROW <terminal-expr> CLOSE-BRACKET)
                     (pos-lift 1 6 `(@update ,$1 ,$3 ,$5)))
                    ;; custom type instantiation
                    ((TYPE OPEN-BRACE <multi-exprs> CLOSE-BRACE)
                     (pos-lift 1 3 `(@instantiate ,$1 ,$3)))
                    ;; type guards
                    ((<apply-expr> IS <type-expr>) (pos-lift 1 3 `(@is ,$1 ,$3)))
                    ((<terminal-expr>) $1))
      ;; terminal expressions
      (<terminal-expr> ((NUM) (pos-lift 1 1 `(@lit-num ,$1)))
                       ((BYTES) (pos-lift 1 1 `(@lit-bytes ,$1)))
                       ((VAR) (pos-lift 1 1 `(@var ,$1)))
                       ((OPEN-PAREN <expr> CLOSE-PAREN) (pos-lift 1 3 $2))
                       ((<vector-expr>) $1)
                       ((UNSAFE CAST <expr> COLON <type-expr>) (pos-lift 1 5
                                                                         `(@unsafe-cast ,$3 ,$5)))
                       ((ANN <expr> COLON <type-expr>) (pos-lift 1 4
                                                                 `(@ann ,$2 ,$4)))
                       ((EXTERN BYTES) (pos-lift 1 2
                                                 `(@extern ,(bytes->string/utf-8 $2))))
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

(module+ test
  (dectx*
   (melo-parse-port (open-input-string #<<EOF
# We have no way of doing this cast safely right now
def self_propagate() = 
	let outputs = env_spender_tx().outputs in
	vlen(outputs) == 1 &&
		(let first_output = ann (unsafe cast env_spender_tx().outputs : [RawCoinData * 1])[0] : RawCoinData in
		first_output[0] == env_self_hash())
		
EOF
                                       ))))