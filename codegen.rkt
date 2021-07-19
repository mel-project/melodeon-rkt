#lang typed/racket
(require "parser.rkt"
         "type-sys/resolver.rkt"
         "type-sys/typecheck.rkt"
         "type-sys/types.rkt"
         "common.rkt")
(require/typed file/sha1
               (bytes->hex-string (-> Bytes String)))
(provide generate-mil)

;; mangle a Melodeon symbol to a Mil symbol
(: mangle-sym (-> Symbol Symbol))
(define (mangle-sym melo-sym)
  ; do nothing atm
  melo-sym)

;; turns a Melodeon @-ast to mil
(: generate-mil (-> @-Ast Any))
(define (generate-mil @-ast)
  (: strip-@ (-> Symbol Symbol))
  (define (strip-@ @-sym)
    (string->symbol (substring (symbol->string @-sym) 1)))

  (match (dectx @-ast)
    ;; casts etc
    [`(@unsafe-cast ,inner ,_) (generate-mil inner)]
    [`(@extern ,str) (string->symbol str)]
    
    ;; let bindings
    [`(@program ,definitions ,body)
     (append (map generate-mil-defs definitions)
             (list (generate-mil body)))]
    [`(@let (,var-name ,var-value) ,body)
     `(let (,(mangle-sym var-name) ,(generate-mil var-value)) ,(generate-mil body))]
    
    ;; logical ops desugar to if-then-else
    [`(@and ,x ,y)
     (define temp-var (gensym 'and))
     `(let (,temp-var ,(generate-mil x))
        (if ,temp-var ,(generate-mil y) ,temp-var))]
    [`(@or ,x ,y)
     (define temp-var (gensym 'and))
     `(let (,temp-var ,(generate-mil x))
        (if ,temp-var ,temp-var ,(generate-mil y)))]
        
    ;; binary ops
    [`(,(? (lambda(op) (member op '(@+ @- @* @/))) op) ,x ,y) `(,(strip-@ op) ,(generate-mil x)
                                                                              ,(generate-mil y))]
    [`(@eq ,x ,y) ; equality generation is special because it's type-dependent
     (generate-eq-mil x y)]
    [`(@var ,(? symbol? varname)) (mangle-sym varname)]
    [`(@lit-num ,(? exact-integer? number)) number]
    [`(@lit-vec ,vv) `(vector . ,(map generate-mil vv))]

    ;; other stuff
    [`(@apply ,fun ,args) `(,(mangle-sym fun) . ,(map generate-mil args))]
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
    [`(@for ,expr ,var-name ,vec-val)
     (let ([count (match (memoized-type vec-val)
                    [(TVectorof _ count) count]
                    [(TVector v) (length v)]
                    [(TBytes b) b])]
           [counter (gensym 'fori)]
           [tempvec (gensym 'forv)])
       `(let (,counter 0 ,tempvec ,(generate-mil vec-val))
          (loop ,count (set-let ()
                                (set! ,tempvec (v-from ,tempvec ,counter
                                                       (let (,var-name (v-get ,tempvec iter))
                                                         ,(generate-mil expr))))
                                (set! ,counter (+ ,counter 1))))
          ,tempvec)
       )]
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
    [`(@def-var ,var ,expr) `(gl ,(mangle-sym var) ,(generate-mil expr))]
    [`(@def-fun ,var ,args ,_ ,expr)
     `(fn ,(mangle-sym var) ,(map mangle-sym (map (inst car Symbol Any) args)) ,(generate-mil expr))]))

;; generates code for equality
(: generate-eq-mil (-> @-Ast @-Ast Any))
(define (generate-eq-mil x y)
  (define x-type (memoized-type x))
  (define y-type (memoized-type y))
  (define bigger-type
    (cond
      [(subtype-of? x-type y-type) y-type]
      [(subtype-of? y-type x-type) x-type]
      [else (error "incomparable types"
                   (type->string x-type)
                   (type->string y-type))]))
  (define x-sym (gensym 'eqx))
  (define y-sym (gensym 'eqy))
  `(let (,x-sym ,(generate-mil x) ,y-sym ,(generate-mil y))
     ,(generate-eq-mil-code
       bigger-type
       x-sym
       y-sym)))

(: generate-eq-mil-code (-> Type Any Any Any))
(define (generate-eq-mil-code type x-sym y-sym)
  (match type
    [(or (TNat)
         (TBin)) `(= ,x-sym ,y-sym)]
    [(TVector lst)
     ;; we generate mil code in a pairwise fashion
     (define lst-len (length lst))
     (define pairwise-eqq
       (for/list ([i lst-len]) : (Listof Any)
         (generate-eq-mil-code (list-ref lst i) `(ref ,x-sym ,i) `(ref ,y-sym ,i))))
     (foldl (lambda ((a : Any) (b : Any))
              `(and ,a ,b))
            1
            pairwise-eqq)]
    [(TVectorof t n)
     (TVector (make-list n t))]
    [(TBytes n)
     (unless (= n 32)
       (error "cannot compare bytestrings of length other than 32"))
     `(= (bytes->u256 ,x-sym)
         (bytes->u256 ,y-sym))]
    [(? Type? t) (error "cannot compare values of non-concrete type"
                        (type->string t))]))

;; demo compiler flow
(module+ main
  (define ast (parameterize ([FILENAME "whatever.melo"])
                (melo-parse-port (open-input-string #<<EOF

let bb = btoi("hello world goodbye world foobar") in
let cc = itob(bb * bb) in
cc == cc
EOF
                                                    ))))
  (displayln "@-Ast:")
  (pretty-print (dectx* ast))
  (displayln "")
  ;; type check
  (@-ast->type ast)
  ;; generate the mil
  (displayln "Mil:")
  (pretty-display
   (generate-mil ast)))