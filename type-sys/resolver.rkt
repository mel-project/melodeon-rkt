#lang typed/racket

(require "types.rkt")
(require/typed "resolver-inner.rkt"
               [subtype-of? (-> Type Type Boolean)])

(provide subtype-of?)