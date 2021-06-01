#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))
(require test-engine/racket-tests)

(provide (contract-out
          (melo-lex-once (-> input-port? position-token?))
          (melo-lex-all (-> input-port? (listof position-token?)))))

(define-tokens value-tokens (NUM VAR))
(define-empty-tokens op-tokens (= OPEN-PAREN CLOSE-PAREN + - * / EOF NEG))

(define-lex-abbrevs
  (lower-letter (:/ "a" "z"))
  (upper-letter (:/ #\A #\Z))
  (digit (:/ "0" "9")))


;; melo-lex-once: Input-Port -> position-token
(define melo-lex-once
  (lexer-src-pos
   ;; if we're at the end we're at the end
   [(eof) 'EOF]
   ;; skip all whitespace
   [(:+ (:or #\tab #\space #\newline)) (return-without-pos (melo-lex-once input-port))]
   ;; pass-through arithmetic operations
   [(:or "=" "+" "-" "*" "/") (string->symbol lexeme)]
   ;; parentheses
   ["(" 'OPEN-PAREN]
   [")" 'CLOSE-PAREN]
   [(concatenation (:+ (:or lower-letter upper-letter))
                   (:* (:or lower-letter upper-letter "_" digit))) (token-VAR (string->symbol lexeme))]
   [(:+ digit) (token-NUM (string->number lexeme))]
   [(:: (:+ digit) #\. (:* digit)) (token-NUM (string->number lexeme))]
   [any-char (error "unexpected character at line" (position-line end-pos) lexeme)]))


;; melo-lex-all: Input-Port -> (Listof position-token)
(define (melo-lex-all input-port (first #t))
  (when first
    (port-count-lines! input-port))
  (match (melo-lex-once input-port)
    [(position-token 'EOF _ _) '()]
    [val (cons val (melo-lex-all input-port #f))]))

