#lang racket/base

(require rackunit
         "parser.rkt"
         "codegen.rkt"
         "type-sys/typecheck.rkt")

(define (prgrm-eq? str prgrm)
  (check-equal?
    (letrec ([@program (melo-parse-port (open-input-string str))]
             [$program (@-transform @program)])
      (generate-mil $program))
     prgrm))

(prgrm-eq?
  "0"
  '(0))

(prgrm-eq?
  "let v = [1,2,3,4] in v[0..2]"
  '((let (v (vector 1 2 3 4)) (v-slice v 0 2))))

(prgrm-eq?
  "[1] * 5"
  '((vector 1 1 1 1 1)))
  ;'((let (x 1 v (v-nil)) (loop 5 (set! v (v-push v x))) v)))
