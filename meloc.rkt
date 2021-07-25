#lang typed/racket
(require "parser.rkt"
         "type-sys/typecheck.rkt"
         "type-sys/types.rkt"
         "common.rkt"
         "codegen.rkt"
         "modules.rkt")

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

(: filename->ast (-> Path-String @-Ast))
(define (filename->ast path)
  (with-input-from-file path
    (lambda ()
      (parameterize ([FILENAME
                      (path->string
                       (path->complete-path path))])
        (melo-parse-port (current-input-port))))))

(module+ main
  (eprintf "parsing...\n")
  (define partial-ast (filename->ast (input-file)))
  (eprintf "demodularizing...\n")
  (define ast (demodularize partial-ast
                            (input-file)
                            filename->ast))
  (pretty-display (dectx* ast))
  (eprintf "typechecking...\n")
  (define type (@-ast->type ast))
  (eprintf "main type: ~a\n" (type->string type))
  (eprintf "generating...\n")
  (define output (generate-mil ast))
  (define output-filename (or (output-file) (path-replace-extension (input-file) ".mil")))
  (with-output-to-file output-filename
    #:exists 'replace
    (lambda()
      (for ([output (in-list output)])
        (pretty-write output)))))