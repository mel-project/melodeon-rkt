#lang racket
(require csp)
;(require memo)
(provide sat-solve)

(define (sat-solve sat)
  ;(pretty-write sat)
  (sat-solve/csp sat))


(define (sat-solve/fast sat)
  (define dnf-form (into-dnf sat))
  (ormap clause-consistent
         (set->list dnf-form)))

(define (clause-consistent clause)
  (not (ormap (λ(elem)
                (match elem
                  [`(not ,x) (set-member? clause x)]
                  [_ #f]))
              (set->list clause))))

(define (into-dnf form)
  (match form
    [`(or ,x ,y) (set-union (into-dnf x)
                            (into-dnf y))]
    [`(and ,x ,y) (list->set
                   (for*/list ([x-clause (into-dnf x)]
                               [y-clause (into-dnf y)])
                     (set-union x-clause y-clause)))]
    ; de morgan's laws
    [`(not (or ,x ,y)) (into-dnf `(and (not ,x) (not ,y)))]
    [`(not (and ,x ,y)) (into-dnf `(or (not ,x) (not ,y)))]
    [`(not (not ,x)) (into-dnf x)]
    [x (set (set x))]))



(define (sat-solve/csp sat)
  ;(pretty-write sat)
  (define csp (sat->csp sat))
  ;(pretty-write csp)
  (define res (solve csp))
  res)


(define DEMO '(not
               (or (not (and (not x-fail) (or x-bin x-nbin)))
                   (and (not x-fail) (not x-fail)))))

(define (sat->csp sat)
  (match-define (cons sat-csp sat-sym) (sat->csp/inner sat))
  (make-csp (csp-vars sat-csp)
            (cons (constraint (list sat-sym) values)
                  (csp-constraints sat-csp))))

(define (sat->csp/inner sat)
  (match sat
    [#f
     (cons (make-csp (list (var 'false (list #f)))
                     empty)
           'false)]
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
                                            (eq? n (proc a b))))
                 (remove-duplicates
                  (append (csp-constraints a-csp)
                          (csp-constraints b-csp)))))
      node)]
    [s
     (cons (make-csp (list (var s (list #t #f)))
                     empty)
           s)]))




(define long-demo
  '(not
    (or (not
         (and (not x-fail)
              (and (not x-fail)
                   (and (or (and (or (and (not x-2-fail) (or x-2-bin x-2-nbin))
                                     (or (and x-2-fail-g5780128 x-2-fail)
                                         (and x-2-fail-baba x-2-fail)))
                                 (or (not
                                      (or (and x-2-fail-g5780128 x-2-fail)
                                          (and x-2-fail-baba x-2-fail)))
                                     (and (not x-2-fail) (or x-2-bin x-2-nbin))))
                            (or (and (or (and (not x-2-fail) (not x-2-fail))
                                         (or (and x-2-fail-g5780128 x-2-fail)
                                             (and x-2-fail-baba x-2-fail)))
                                     (or (not
                                          (or (and x-2-fail-g5780128 x-2-fail)
                                              (and x-2-fail-baba x-2-fail)))
                                         (and (not x-2-fail)
                                              (or x-2-bin x-2-nbin))))
                                (or (and (or (and (not x-2-fail)
                                                  (or x-2-bin x-2-nbin))
                                             (or (and x-2-fail-g5780128 x-2-fail)
                                                 (and x-2-fail-baba x-2-fail)))
                                         (or (not
                                              (or (and x-2-fail-g5780128 x-2-fail)
                                                  (and x-2-fail-baba x-2-fail)))
                                             (and (not x-2-fail)
                                                  (or x-2-bin x-2-nbin))))
                                    (and (not x-2-fail) #f))))
                        (and (or (and (or (and (not x-1-fail)
                                               (or x-1-bin x-1-nbin))
                                          (or (and x-1-fail-g5780128 x-1-fail)
                                              (and x-1-fail-baba x-1-fail)))
                                      (or (not
                                           (or (and x-1-fail-g5780128 x-1-fail)
                                               (and x-1-fail-baba x-1-fail)))
                                          (and (not x-1-fail)
                                               (or x-1-bin x-1-nbin))))
                                 (or (and (or (and (not x-1-fail) (not x-1-fail))
                                              (or (and x-1-fail-g5780128 x-1-fail)
                                                  (and x-1-fail-baba x-1-fail)))
                                          (or (not
                                               (or (and x-1-fail-g5780128
                                                        x-1-fail)
                                                   (and x-1-fail-baba x-1-fail)))
                                              (and (not x-1-fail)
                                                   (or x-1-bin x-1-nbin))))
                                     (or (and (or (and (not x-1-fail)
                                                       (or x-1-bin x-1-nbin))
                                                  (or (and x-1-fail-g5780128
                                                           x-1-fail)
                                                      (and x-1-fail-baba
                                                           x-1-fail)))
                                              (or (not
                                                   (or (and x-1-fail-g5780128
                                                            x-1-fail)
                                                       (and x-1-fail-baba
                                                            x-1-fail)))
                                                  (and (not x-1-fail)
                                                       (or x-1-bin x-1-nbin))))
                                         (and (not x-1-fail) #f))))
                             (and (or (and (or (and (not x-0-fail)
                                                    (or x-0-bin x-0-nbin))
                                               (or (and x-0-fail-g5780128
                                                        x-0-fail)
                                                   (and x-0-fail-baba x-0-fail)))
                                           (or (not
                                                (or (and x-0-fail-g5780128
                                                         x-0-fail)
                                                    (and x-0-fail-baba x-0-fail)))
                                               (and (not x-0-fail)
                                                    (or x-0-bin x-0-nbin))))
                                      (or (and (or (and (not x-0-fail)
                                                        (not x-0-fail))
                                                   (or (and x-0-fail-g5780128
                                                            x-0-fail)
                                                       (and x-0-fail-baba
                                                            x-0-fail)))
                                               (or (not
                                                    (or (and x-0-fail-g5780128
                                                             x-0-fail)
                                                        (and x-0-fail-baba
                                                             x-0-fail)))
                                                   (and (not x-0-fail)
                                                        (or x-0-bin x-0-nbin))))
                                          (or (and (or (and (not x-0-fail)
                                                            (or x-0-bin x-0-nbin))
                                                       (or (and x-0-fail-g5780128
                                                                x-0-fail)
                                                           (and x-0-fail-baba
                                                                x-0-fail)))
                                                   (or (not
                                                        (or (and x-0-fail-g5780128
                                                                 x-0-fail)
                                                            (and x-0-fail-baba
                                                                 x-0-fail)))
                                                       (and (not x-0-fail)
                                                            (or x-0-bin
                                                                x-0-nbin))))
                                              (and (not x-0-fail) #f))))
                                  x-vec-3))))))
        (and (not x-fail) (not x-fail)))))