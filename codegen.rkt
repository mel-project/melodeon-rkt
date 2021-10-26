#lang typed/racket
(require "grammar/parser.rkt"
         "type-sys/typecheck-unify.rkt"
         "type-sys/typecheck.rkt"
         "type-sys/type-bag.rkt"
         "type-sys/types.rkt"
         "asts/typed-ast.rkt"
         "asts/raw-ast.rkt")
(require/typed file/sha1
               (bytes->hex-string (-> Bytes String)))
(provide generate-mil)

;; mangle a Melodeon symbol to a Mil symbol
(: mangle-sym (-> Symbol Symbol))
(define (mangle-sym melo-sym)
  ; do nothing atm
  melo-sym)

; generates a top-level program into mil code
(: generate-mil (-> $program Any))
(define (generate-mil prgrm)
  (append
   (map generate-mil-def ($program-fun-defs prgrm))
   (list (generate-mil-expr ($program-expr prgrm)))))

;; turns a Melodeon $-ast to mil
(: generate-mil-expr (-> $-Ast Any))
(define (generate-mil-expr $-ast)
  (match ($-Ast-node $-ast)
    ;; let bindings
    [($let var-name var-value body)
     `(let (,(mangle-sym var-name) ,(generate-mil-expr var-value)) ,(generate-mil-expr body))]
    ;; binary ops
    [($bin op x y)
     `(,op ,(generate-mil-expr x) ,(generate-mil-expr y))]
    [($eq x y) ; equality generation is special because it's type-dependent
     (generate-eq-mil x y)]
    [($var varname) (mangle-sym varname)]
    [($lit-num n) n]
    [($lit-vec vv) `(vector . ,(map generate-mil-expr vv))]

    ;; other stuff
    [($apply fun args) `(,(mangle-sym fun) . ,(map generate-mil-expr args))]
    [($push v x) `(,(match (bag->type (type->bag ($-Ast-type v)))
                        [(TVectorof _ _) 'v-push]
                        [(TVector _) 'v-push]
                        [(TBytes _) 'b-push])
                     ,(generate-mil-expr x)
                     ,(generate-mil-expr v))]
    [($cons x v) `(,(match (bag->type (type->bag ($-Ast-type v)))
                        [(TVectorof _ _) 'v-cons]
                        [(TVector _) 'v-cons]
                        [(TBytes _) 'b-cons])
                     ,(generate-mil-expr x)
                     ,(generate-mil-expr v))]
    [($append x y) `(,(match (bag->type (type->bag ($-Ast-type x)))
                        [(TVectorof _ _) 'v-concat]
                        [(TVector _) 'v-concat]
                        [(TBytes _) 'b-concat])
                     ,(generate-mil-expr x)
                     ,(generate-mil-expr y))]
    [($index vec idx) `(,(match ($-Ast-type vec)
                           [(TTagged _ _) 'v-get]
                           [(TVectorof _ _) 'v-get]
                           [(TVector _) 'v-get]
                           [(TBytes _) 'b-get]) ,(generate-mil-expr vec)
                                                ,(generate-mil-expr idx))]
    [($slice vec from to) `(,(match ($-Ast-type vec)
                               [(TTagged _ _) 'v-slice]
                               [(TVectorof _ _) 'v-slice]
                               [(TVector _) 'v-slice]
                               [(TBytes _) 'b-slice])
                            ,(generate-mil-expr vec)
                            ,(generate-mil-expr from)
                            ,(generate-mil-expr to))]
    ; Generate a vector of numbers from..to
    [($range $from $to)
     (match-define ($-Ast _ ($lit-num from)) $from)
     (match-define ($-Ast _ ($lit-num to)) $to)
     `(let (v (v-nil) i 0)
        (loop ,(- to from)
          (set! (v-push v i))
          (set! i (+ i 1)))
        v)]
    [($init-vec expr size)
     `(let (x ,(generate-mil-expr expr)
              v (v-nil))
        (loop ,size
              (set! v (v-push v x)))
        v)]
    [($if x y z) `(if ,(generate-mil-expr x)
                      ,(generate-mil-expr y)
                      ,(generate-mil-expr z))]
    [($fold expr var acc-var ini-val vec)
     (let ([count (match ($-Ast-type vec)
                    [(TVectorof _ count) count]
                    [(TVector v) (length v)]
                    [(TBytes b) b])])
       `(let (v ,(generate-mil-expr vec)
              acc ,(generate-mil-expr ini-val)
              i 0)
          (loop ,count
            (set-let ()
              (set! acc
                (let (,var (v-get v i)
                      ,acc-var acc)
                  ,(generate-mil-expr expr)))
              (set! i (+ i 1))))
          acc))]
    [($for expr var-name vec-val)
     (let ([count (match ($-Ast-type vec-val)
                    [(TVectorof _ count) count]
                    [(TVector v) (length v)]
                    [(TBytes b) b])]
           [counter (gensym 'i)]
           [tempvec (gensym 'v)])
       `(let (,counter 0 ,tempvec ,(generate-mil-expr vec-val))
          (loop ,count (set-let ()
                                (set! ,tempvec (v-from ,tempvec ,counter
                                                       (let (,var-name (v-get ,tempvec iter))
                                                         ,(generate-mil-expr expr))))
                                (set! ,counter (+ ,counter 1))))
          ,tempvec)
       )]
    [($is expr type)
     (define tmpsym (gensym 'is))
     `(let (,tmpsym ,(generate-mil-expr expr))
        ,(generate-is type tmpsym))]
    [($lit-bytes bts) (string->symbol
                       (string-append "0x"
                                      (bytes->hex-string bts)))]
    [($block inner) `(let () . ,(map generate-mil-expr inner))]
    [($loop n body) `(loop ,n ,(generate-mil-expr body))]
    [($extern s) (string->symbol s)]
    [($extern-call f body) `(,(string->symbol f) . ,(map generate-mil-expr body))]
    [other (error "invalid $-ast" other)]))

(: generate-mil-def (-> $fndef Any))
(define (generate-mil-def def)
  (match def
    [($fndef name binds body)
     `(fn ,(mangle-sym name)
          ,(map mangle-sym (map (inst car Symbol Any) binds))
          ,(generate-mil-expr body))]))
; TODO mil does not have globals
;[($vardef var expr)
; `(gl ,(mangle-sym var) ,(generate-mil expr))]

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
  `(let (,x-sym ,(generate-mil-expr x) ,y-sym ,(generate-mil-expr y))
     ,(generate-eq-mil-code
       bigger-type
       x-sym
       y-sym)))

(: generate-eq-mil-code (-> Type Any Any Any))
(define (generate-eq-mil-code type x-sym y-sym)
  (match type
    [(TNat) `(= ,x-sym ,y-sym)]
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
    [(TVectorof t e)
     (define n (int-or-err e))
     (TVector (make-list n t))]
    [(TBytes e)
     (define n (int-or-err e))
     (unless (= n 32)
       (error "cannot compare bytestrings of length other than 32"))
     `(= (bytes->u256 ,x-sym)
         (bytes->u256 ,y-sym))]
    [(? Type? t) (error "cannot compare values of non-concrete type"
                        (type->string t))]))

(: int-or-err (-> Const-Expr Integer))
(define (int-or-err e)
   (define n (normal-form e))
   (if (integer? n)
     n
     (error (format "Constant expression did not resolve to a concrete form:
                    ~a" e))))

(: generate-is (-> Type Any Any)) 
(define (generate-is type sym)
  (match type
    [(TNat) `(= (typeof ,sym) 0)]
    [(TBytes n) `(if (= (typeof ,sym) 1)
                     (= (blength ,sym) ,n)
                     0)]
    [(TVectorof t e)
     (define n (int-or-err e))
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
def labooyah() = 2
def dup<T>(x: T) = [x, x]
def getfirst<T>(x : [T, T]) = x[0]
def roundtrip<T>(x: T) = getfirst(dup(x))
- - - 
roundtrip([1, 2, 3])[0] + roundtrip(5) + 6 + labooyah()
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