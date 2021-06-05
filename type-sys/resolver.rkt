#lang racket
(require racklog)
(require "../common.rkt")

;; Prolog-style type resolver

(define %lt
  (%rel (x y)
    [(x (sub1 y))]
    [(x (%lt x (sub1 y)))]))