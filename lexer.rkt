#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide (contract-out
          (melo-lex-once (-> input-port? position-token?))
          (melo-lex-all (-> input-port? (listof position-token?))))
         value-tokens
         syntax-tokens)

(define-tokens value-tokens (NUM VAR FUN TYPE))
(define-empty-tokens syntax-tokens (= OPEN-PAREN CLOSE-PAREN OPEN-BRACKET CLOSE-BRACKET OPEN-BRACE CLOSE-BRACE COMMA + - * / EOF NEG
                                      LET IN
                                      COLON
                                      SEMICOLON
                                      ARROW
                                      BEGIN END
                                      THEN
                                      DEF))

(define-lex-abbrevs
  (lower-letter (:/ "a" "z"))
  (upper-letter (:/ #\A #\Z))
  (digit (:/ "0" "9")))


;; melo-lex-once: Input-Port -> position-token
(define melo-lex-once
  (lexer-src-pos
   ;; if we're at the end we're at the end
   [(eof) 'EOF]
   ;; keywords
   ["let" 'LET]
   ["in" 'IN]
   ["def" 'DEF]
   ["begin" 'BEGIN]
   [":" 'COLON]
   ["end" 'END]
   ;; punctuation
   ["," 'COMMA]
   [";" 'SEMICOLON]
   ["->" 'ARROW]
   ;; skip all whitespace
   [(:+ (:or #\tab #\space #\newline)) (return-without-pos (melo-lex-once input-port))]
   ;; pass-through arithmetic operations
   [(:or "=" "+" "-" "*" "/") (string->symbol lexeme)]
   ;; parentheses
   ["(" 'OPEN-PAREN]
   [")" 'CLOSE-PAREN]
   ["[" 'OPEN-BRACKET]
   ["]" 'CLOSE-BRACKET]
   ["{" 'OPEN-BRACE]
   ["}" 'CLOSE-BRACE]
   ;; literals
   [(concatenation upper-letter
                   (:* (:or lower-letter upper-letter digit))) (token-TYPE (string->symbol lexeme))]
   [(concatenation "@"
                   lower-letter
                   (:* (:or lower-letter upper-letter "_" digit))) (token-FUN (string->symbol lexeme))]
   [(concatenation lower-letter
                   (:* (:or lower-letter upper-letter "_" digit))) (token-VAR (string->symbol lexeme))]
   [(:+ digit) (token-NUM (string->number lexeme))]
   [(:: (:+ digit) #\. (:* digit)) (token-NUM (string->number lexeme))]
   ;; error
   [any-char (error "unexpected character at line" (position-line end-pos) lexeme)]))


;; melo-lex-all: Input-Port -> (Listof position-token)
(define (melo-lex-all input-port (first #t))
  (when first
    (port-count-lines! input-port))
  (match (melo-lex-once input-port)
    [(position-token 'EOF _ _) '()]
    [val (cons val (melo-lex-all input-port #f))]))