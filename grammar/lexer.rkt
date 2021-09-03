#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         file/sha1)

(provide (contract-out
          (melo-lex-once (-> input-port? position-token?))
          (melo-lex-all (-> input-port? (listof position-token?))))
         value-tokens
         syntax-tokens)

(define-tokens value-tokens (NUM VAR FUN TYPE BYTES))
(define-empty-tokens syntax-tokens (OPEN-PAREN CLOSE-PAREN OPEN-BRACKET
                                    CLOSE-BRACKET LESS-THAN GREATER-THAN
                                    OPEN-BRACE CLOSE-BRACE COMMA EOF NEG
                                    = == ++ + - * / OR XOR AND ETC RANGE
                                    SHL SHR BAND BOR
                                    TWOSTARS
                                    ---
                                    TAND
                                    TNEG
                                    TOR
                                    PERCENT
                                    LET IN
                                    COLON HASH
                                    SEMICOLON
                                    REQUIRE
                                    PROVIDE
                                    UNSAFE
                                    CAST
                                    WHERE
                                    PIPE
                                    ANN
                                    DO
                                    DOT
                                    IS
                                    EXTERN
                                    CALL
                                    DONE
                                    FOR
                                    FOLD
                                    FROM
                                    DEF
                                    STRUCT
                                    LOOP
                                    FAT-ARROW
                                    ALIAS
                                    IF
                                    THEN
                                    ELSE
                                    SET!))

(define-lex-abbrevs
  (lower-letter (:/ "a" "z"))
  (upper-letter (:/ #\A #\Z))
  (digit (:/ "0" "9"))
  (hex-digit (:or (:/ "0" "9")
                  (:/ "a" "f"))))


;; melo-lex-once: Input-Port -> position-token
(define melo-lex-once
  (lexer-src-pos
   ;; if we're at the end we're at the end
   [(eof) 'EOF]
   ;; keywords
   ["let" 'LET]
   ["in" 'IN]
   ["def" 'DEF]
   ["struct" 'STRUCT]
   ["call" 'CALL]
   [":" 'COLON]
   ["=>" 'FAT-ARROW]
   ["where" 'WHERE]
   ["unsafe" 'UNSAFE]
   ["alias" 'ALIAS]
   ["require" 'REQUIRE]
   ["provide" 'PROVIDE]
   ["||" 'OR]
   ["|" 'BOR]
   ["^" 'XOR]
   ["&&" 'AND]
   ["&" 'BAND]
   ["<<" 'SHL]
   [">>" 'SHR]
   ["not" 'NOT]
   ["cast" 'CAST]
   ["loop" 'LOOP]
   ["ann" 'ANN]
   ["if" 'IF]
   ["then" 'THEN]
   ["else" 'ELSE]
   ["do" 'DO]
   ["done" 'DONE]
   ["for" 'FOR]
   ["fold" 'FOLD]
   ["from" 'FROM]
   ["set!" 'SET!]
   ["extern" 'EXTERN]
   ["is" 'IS]
   ;; punctuation
   ["," 'COMMA]
   ["..." 'ETC]
   [".." 'RANGE]
   ["." 'DOT]
   ["<" 'LESS-THAN]
   [">" 'GREATER-THAN]
   ["#" 'HASH]
   ["%" 'PERCENT]
   [";" 'SEMICOLON]
   ;["|" 'TOR]
   ;["&" 'TAND]
   ;["~" 'TNEG]
   ["**" 'TWOSTARS]
   [(:: "-" (:* #\space) "-" (:* #\space) "-") '---]
   ;; skip comments
   [(:: "#" (:* (:~ #\newline))) (return-without-pos (melo-lex-once input-port))]
   ;; skip all whitespace
   [(:+ (:or #\tab #\space #\newline)) (return-without-pos (melo-lex-once input-port))]
   ;; pass-through arithmetic operations
   [(:or "=" "+" "-" "*" "/" "++" "==") (string->symbol lexeme)]
   ;; parentheses
   ["(" 'OPEN-PAREN]
   [")" 'CLOSE-PAREN]
   ["[" 'OPEN-BRACKET]
   ["]" 'CLOSE-BRACKET]
   ["{" 'OPEN-BRACE]
   ["}" 'CLOSE-BRACE]
   ;; literals
   [(:: upper-letter
        (:* (:or lower-letter upper-letter digit))) (token-TYPE (string->symbol lexeme))]
   [(:: lower-letter
        (:* (:or lower-letter upper-letter "_" digit))) (token-VAR (string->symbol lexeme))]
   [(:+ digit) (token-NUM (string->number lexeme))]
   ;[(:: (:+ digit) #\. (:* digit)) (token-NUM (string->number lexeme))]
   [(:: "x"
        "\""
        (:* (:: hex-digit
                hex-digit))
        "\"") (token-BYTES (hex-string->bytes (substring lexeme 2
                                                         (- (string-length lexeme) 1))))]
   [(:: "\""
        (:* (:~ "\""))
        "\"") (token-BYTES (string->bytes/utf-8 (substring lexeme 1
                                                           (- (string-length lexeme) 1))))]
   ;; error
   [any-char (error "unexpected character at line" (position-line end-pos) lexeme)]))


;; melo-lex-all: Input-Port -> (Listof position-token)
(define (melo-lex-all input-port (first #t))
  (when first
    (port-count-lines! input-port))
  (match (melo-lex-once input-port)
    [(position-token 'EOF _ _) '()]
    [val (cons val (melo-lex-all input-port #f))]))