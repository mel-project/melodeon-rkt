#lang typed/racket
(require "parser.rkt"
         "type-sys/resolver.rkt"
         "type-sys/typecheck.rkt"
         "type-sys/types.rkt"
         "typed-ast.rkt"
         "common.rkt")
(require/typed file/sha1
               (bytes->hex-string (-> Bytes String)))
(provide generate-mil)

;; mangle a Melodeon symbol to a Mil symbol
(: mangle-sym (-> Symbol Symbol))
(define (mangle-sym melo-sym)
  ; do nothing atm
  melo-sym)

;; turns a Melodeon $-ast to mil
(: generate-mil (-> $program Any))
(define (generate-mil $-ast)
  ;; TODO generate fns

  (match ($program-expr $-ast)
    ;; let bindings
    [($let var-name var-value body)
     `(let (,(mangle-sym var-name) ,(generate-mil var-value)) ,(generate-mil body))]
    ;; binary ops
    [($bin op x y)
     `(,op ,(generate-mil x) ,(generate-mil y))]
    [($eq x y) ; equality generation is special because it's type-dependent
     (generate-eq-mil x y)]
    [($var varname) (mangle-sym varname)]
    [($lit-num n) n]
    [($lit-vec vv) `(vector . ,(map generate-mil vv))]

    ;; other stuff
    [($apply fun args) `(,(mangle-sym fun) . ,(map generate-mil args))]
    [($append x y) `(,(match ($-Ast-type x)
                        [(TVectorof _ _) 'v-concat]
                        [(TVector _) 'v-concat]
                        [(TBytes _) 'b-concat])
                        ,(generate-mil x)
                        ,(generate-mil y))]
    [($index vec idx) `(,(match ($-Ast-type vec)
                           [(TVectorof _ _) 'v-get]
                           [(TVector _) 'v-get]
                           [(TBytes _) 'b-concat]) ,(generate-mil vec)
                                                   ,(generate-mil idx))]
    [($if x y z) `(if ,(generate-mil x)
                      ,(generate-mil y)
                      ,(generate-mil z))]
    [($for expr var-name vec-val)
     (let ([count (match ($-Ast-type vec-val)
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
    [($is expr type)
     (define tmpsym (gensym 'is))
     `(let (,tmpsym ,(generate-mil expr))
        ,(generate-is type tmpsym))]
    [($lit-bytes bts) (string->symbol
                        (string-append "0x"
                                       (bytes->hex-string bts)))]
    [($block inner) `(let () . ,(map generate-mil inner))]
    [($loop n body) `(loop ,n ,(generate-mil body))]
    [other (error "invalid @-ast" other)]))

#|
(: generate-mil-defs (-> Definition Any))
(define (generate-mil-defs def)
  (match def
    [`(@def-var ,var ,expr) `(gl ,(mangle-sym var) ,(generate-mil expr))]
    [`(@def-fun ,var ,args ,_ ,expr)
     `(fn ,(mangle-sym var) ,(map mangle-sym (map (inst car Symbol Any) args)) ,(generate-mil expr))]))
|#

;; generates code for equality
(: generate-eq-mil (-> $-Ast $-Ast Any))
(define (generate-eq-mil x y)
  (define x-type ($-Ast-type x))
  (define y-type ($-Ast-type y))
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
         (generate-eq-mil-code (list-ref lst i) `(v-get ,x-sym ,i) `(v-get ,y-sym ,i))))
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

(: generate-is (-> Type Any Any))
(define (generate-is type sym)
  (match type
    [(TBin) (define tmp-name (gensym 'bin))
            `(let (,tmp-name ,(generate-is (TNat) sym))
               (if ,tmp-name (< ,sym 2) 0))]
    [(TNat) `(= (typeof ,sym) 0)]
    [(TBytes n) `(if (= (typeof ,sym) 1)
                     (= (blength ,sym) ,n)
                     0)]
    [(TVectorof t n)
     (generate-is (TVector (make-list n t)) sym)]
    [(TVector types)
     (define pairwise-is
       (for/list ([i (length types)]
                  [type types]) : (Listof Any)
         (generate-is type `(v-get ,sym ,i))))
     `(if (= (typeof ,sym) 2)
          ,(foldl (lambda ((a : Any) (b : Any))
                    `(and ,a ,b))
                  1
                  pairwise-is)
          0)]
    ))

;; demo compiler flow
(module+ main
  (define ast (parameterize ([FILENAME "whatever.melo"])
                (melo-parse-port (open-input-string #<<EOF

let x = ann 0 : Nat in
if x is Bin then
  x + 10
else
  x * 10
EOF
                                                    ))))
  (displayln "@-Ast:")
  (pretty-print (dectx* ast))
  (displayln "")
  ;; type check
  (define prgrm (@-transform ast))
  ;; generate the mil
  (displayln "Mil:")
  (pretty-display
   (generate-mil prgrm)))