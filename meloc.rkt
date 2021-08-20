#lang typed/racket
(require "grammar/parser.rkt"
         "type-sys/typecheck.rkt"
         "type-sys/types.rkt"
         "asts/raw-ast.rkt"
         "monomorphize.rkt"
         "asts/typed-ast.rkt"
         "codegen.rkt"
         "modules.rkt"
         racket/runtime-path)

(define-runtime-path STDLIB-LOCATION "stdlib.melo")

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
  (define partial-ast (match (dectx (filename->ast (input-file)))
                        [`(@program ,defs ,expr)
                         `(@program ,(cons `(@require ,(path->string STDLIB-LOCATION)) defs) ,expr)]))
  (eprintf "demodularizing...\n")
  (define ast (demodularize partial-ast
                            (input-file)
                            filename->ast))
  (pretty-write (dectx* ast))
  (eprintf "generating $-Ast...\n")
  (define res (@-transform ast))
  ;(pretty-write res)
  (eprintf "main type: ~a\n" (type->string ($-Ast-type ($program-expr res))))
  (eprintf "generating mil...\n")
  (define output (generate-mil res))
  (define output-filename (or (output-file) (path-replace-extension (input-file) ".mil")))
  (with-output-to-file output-filename
    #:exists 'replace
    (lambda()
      (for ([output (in-list output)])
        (pretty-write output)))))