#lang racket
(require "lexer.rkt")
(require parser-tools/lex)
(require parser-tools/yacc)
(require compatibility/defmacro)
;; we only provide melo-parse-port because it has a much saner API
(provide (contract-out
          (melo-parse-port (-> input-port? any)))
         FILENAME)

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
            )
    ;; let x = y in expr
    (<let-expr> ((LET <comma-separated-defs> IN <expr>) (pos-lift 1 4
                                                                  `(@let ,$2 ,$4))))
    (<comma-separated-defs> ((VAR = <expr> COMMA <comma-separated-defs>) (hash-set $5 $1 $3))
                            ((VAR = <expr>) (hash $1 $3)))
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

(define-macro (liftpsn comb a b . rst)
  `(,comb (psn ,(string->symbol (format "$~a-start-pos" a)))
          (psn ,(string->symbol (format "$~a-end-pos" b)))
          . ,rst))

(define-macro (pos-lift a b expr)
  `(let ([poz-a ,(string->symbol (format "$~a-start-pos" a))]
         [poz-b ,(string->symbol (format "$~a-end-pos" b))])
     (datum->syntax
      #f
      ,expr
      (vector
       (FILENAME)
       (position-line poz-a)
       (position-col poz-b)
       (position-offset poz-a)
       (- (position-offset poz-b)
          (position-offset poz-a))
       ))))

(module+ main
  (melo-parse-port (open-input-string "
let x = 123,
    y = 456 in
    x + y
")))