#lang racket
(require csp)
(provide sat-solve)

(define (sat-solve sat)
  (solve (sat->csp sat)))

(define (sat->csp sat)
  (match-define (cons sat-csp sat-sym) (sat->csp/inner sat))
  (make-csp (csp-vars sat-csp)
            (cons (constraint (list sat-sym) values)
                  (csp-constraints sat-csp))))

(define (sat->csp/inner sat)
  (match sat
    [(? boolean? b)
     (cons (make-csp (list (var (if b 'true 'false) (list b)))
                     (list (constraint (list (if b 'true 'false)) (λ(_) b))))
           'true)]
    [`(not ,a)
     (match-define (cons a-csp a-sym) (sat->csp/inner a))
     (define node (gensym 'not))
     (cons
      (make-csp (cons (var node (list #t #f))
                      (csp-vars a-csp))
                (cons (constraint (list node a-sym) (λ(a b) (equal? a (not b))))
                      (csp-constraints a-csp)))
      node)]
    [`(,(? (λ(x) (or (eq? x 'or) (eq? x 'and))) sym) ,a ,b)
     (match-define (cons a-csp a-sym) (sat->csp/inner a))
     (match-define (cons b-csp b-sym) (sat->csp/inner b))
     (define node (gensym sym))
     (define vars (remove-duplicates
                   (cons (var node (list #t #f))
                         (append (csp-vars a-csp)
                                 (csp-vars b-csp)))))
     (define proc (match sym
                    ['or (λ(a b) (or a b))]
                    ['and (λ(a b) (and a b))]))
     (cons
      (make-csp vars
                (cons
                 (constraint (list node
                                   a-sym
                                   b-sym) (λ(n a b)
                                            (equal? n (proc a b))))
                 (remove-duplicates
                  (append (csp-constraints a-csp)
                          (csp-constraints b-csp)))))
      node)]
    [s
     (cons (make-csp (list (var s (list #t #f)))
                     empty)
           s)]))