#lang typed/racket
(require "parser.rkt"
         "type-sys/typecheck.rkt"
         "type-sys/types.rkt"
         "common.rkt")
(require/typed file/sha1
               (bytes->hex-string (-> Bytes String)))
(provide generate-mil)


(: generate-mil (-> @-Ast Any))
(define (generate-mil @-ast)
  (: strip-@ (-> Symbol Symbol))
  (define (strip-@ @-sym)
    (string->symbol (substring (symbol->string @-sym) 1)))

  (match (dectx @-ast)
    ;; let bindings
    [`(@program ,definitions ,body)
     (append (map generate-mil-defs definitions)
             (list (generate-mil body)))]
    [`(@let (,var-name ,var-value) ,body)
     `(let (,var-name ,(generate-mil var-value)) ,(generate-mil body))]
    ;; binary ops
    [`(,(? (lambda(op) (member op '(@+ @- @* @/))) op) ,x ,y) `(,(strip-@ op) ,(generate-mil x)
                                                                              ,(generate-mil y))]
    [`(@var ,(? symbol? varname)) varname]
    [`(@lit-num ,(? exact-integer? number)) number]
    [`(@lit-vec ,vv) `(vector . ,(map generate-mil vv))]
    [`(@apply ,fun ,args) `(,fun . ,(map generate-mil args))]
    [`(@append ,x ,y) `(,(match (memoized-type x)
                           [(TVectorof _ _) 'v-concat]
                           [(TVector _) 'v-concat]
                           [(TBytes _) 'b-concat])
                        ,(generate-mil x)
                        ,(generate-mil y))]
    [`(@index ,vec ,idx) `(,(match (memoized-type vec)
                              [(TVectorof _ _) 'v-get]
                              [(TVector _) 'v-get]
                              [(TBytes _) 'b-concat]) ,(generate-mil vec)
                                                     ,(generate-mil idx))]
    [`(@if ,x ,y ,z) `(if ,(generate-mil x)
                          ,(generate-mil y)
                          ,(generate-mil z))]
    ; TODO mangle i using @
    [`(@for ,expr ,var-name ,vec-val)
      (let ([count (match (memoized-type vec-val)
                     [(TVectorof _ count) count]
                     [(TVector v) (length v)]
                     [(TBytes b) b])])
        `(let (iter 0 v ,(generate-mil vec-val))
           (loop ,count
             (set! v (v-from v iter
                       (let (,var-name (v-get v iter))
                         ,(generate-mil expr)))))
           v))]
        ;`(let (res ,vec-val
        ;       i 0)
        ;   (loop ,count
        ;     (set-let ()
               ;,(generate-mil (list-replace expr var-name '(v-get res i)))
;               ,(generate-mil (list-replace
;                                expr
;                                `(@var ,var-name)
;                                `(@index (@var 'res) (@var 'i))))
        ;       (set! res (v-from res i (v-get res i)))
        ;       (set! i (+ i 1))))))]

    [`(@lit-bytes ,bts) (string->symbol
                         (string-append "0x"
                                        (bytes->hex-string bts)))]
    [`(@ann ,inner ,_) (generate-mil inner)]
    [`(@block ,inner) `(let () . ,(map generate-mil inner))]
    [`(@set! ,x ,y) `(set! ,x ,(generate-mil y))]
    [`(@loop ,n ,body) `(loop ,n ,(generate-mil body))]
    [other (error "invalid @-ast" other)]))

(: generate-mil-defs (-> Definition Any))
(define (generate-mil-defs def)
  (match def
    [`(@def-var ,var ,expr) `(gl ,var ,(generate-mil expr))]
    [`(@def-fun ,var ,args ,_ ,expr)
     `(fn ,var ,(map (inst car Symbol Any) args) ,(generate-mil expr))]))

#|
(: list-replace (-> (Listof Any) (Listof Any) (Listof Any) (Listof Any)))
(define (list-replace l old-sym new-sym)
  (map
    (lambda (x) (if (eq? x old-sym) new-sym x))
    ;(lambda (x) (match x
      ;[`(@var ,sym) (if (eq? sym old-sym) `(@var ,new-sym) `(@var ,old-sym))]
      ;[(var a) a]))
    l))
|#


#|
;(map-ast expr (lambda (x)
;                (match x
;                  [`(@var ,x) `(@index (@var ,vec) (@var ,i))]
;                  [(var a) a])))

;(: subst-var (-> @-Ast Symbol Symbol @-Ast))
(: map-ast (-> @-Ast (-> @-Ast @-Ast) @-Ast))
;(define (subst-var expr old-sym new-sym)
(define (map-ast expr f)
  ; f is applied to an ast without context, but context is preserved
  (define (f (lambda (x)
               (let ([ctx (get-context x)])
                 (if ctx
                   (with-context ctx (f (dectx x)))
                   (f x))))))
  (define (fmap-ast (compose-app f map-ast)))
  (match expr
    [`(@let (_ ,e) ,body) `(@let (_ ,(fmap-ast e)) ,(fmap-ast body))]
    [`(@-Binop ,l ,r) `(@-Binop ,(fmap-ast l) ,(fmap-ast r))]
    [`(@lit-vec ,v) `(@lit-vec ,(map fmap-ast v))]
    ;[`(@program (Listof Definition) @-Ast)]
    [`(@apply ,sym ,exprs) `(@apply ,sym ,(map fmap-ast exprs))]
    [`(@block ,exprs) `(@block ,(map fmap-ast exprs))]
    [`(@index vec i) `(@index ,(fmap-ast vec) ,(fmap-ast i))]
;     [`(@update @-Ast @-Ast @-Ast)]
;     [`(@unsafe-cast @-Ast Type-Expr)]
;     [`(@ann @-Ast Type-Expr)]
;     [`(@if @-Ast @-Ast @-Ast)]
;     [`(@for @-Ast Symbol @-Ast)]
;     [`(@lit-bytes Bytes)]
;     [`(@set! Symbol @-Ast)]
;     [`(@set! Symbol @-Ast)]
;     [`(@loop Nonnegative-Integer @-Ast)]
    ;[`(@lit-num Nonnegative-Integer)]
    ;[`(@var ,(? symbol? varname)) `(@var ,new-sym)]
     ; (with-context @-Ast)))
    [(var x) x]))
|#

;; demo compiler flow
(module+ main
  (define ast (parameterize ([FILENAME "whatever.melo"])
                (melo-parse-port (open-input-string #<<EOF
def laboo(yah : [Nat * 6]) = [yah[0]] ++ [yah[1]]


if 1 then 2 else "labooyah" ++ "hello world" ++ x"deadbeef"

EOF
                                                    ))))
  (displayln "@-Ast:")
  (pretty-print (dectx* ast))
  (displayln "")
  ;; type check
  (void (@-ast->type ast))
  ;; generate the mil
  (displayln "Mil:")
  (pretty-display
   (generate-mil ast)))