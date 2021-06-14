#lang typed/racket
(require "parser.rkt"
         "type-sys/typecheck.rkt"
         "type-sys/types.rkt"
         "common.rkt"
         "codegen.rkt")

(: input-file (Parameter String))
(define input-file (make-parameter ""))

(: output-file (Parameter (Option String)))
(define output-file (make-parameter #f))

(input-file
 (cast 
  (command-line
   #:program "meloc"
   #:args (filename) ; expect one command-line argument: <filename>
   ; return the argument as a filename to compile
   filename)
  String))

(module+ main
  (define input-port (open-input-file
                      (input-file)
                      #:mode 'text))
  (eprintf "parsing...\n")
  (define ast (parameterize ([FILENAME
                              (path->string
                               (path->complete-path (input-file)))])
                (melo-parse-port input-port)))
  (eprintf "typechecking...\n")
  (define type (@-ast->type ast))
  (eprintf "main type: ~a\n" (type->string type))
  (eprintf "generating...\n")
  (define output (generate-mil ast))
  (for ([output (in-list output)])
    (pretty-write output)))