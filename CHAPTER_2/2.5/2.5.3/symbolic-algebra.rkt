#lang scheme

(require "./general-arithmetic.rkt")


(define x (make-rational 1 4))
(define z1 (make-complex-from-real-imag 3 5))
(define z2 (make-complex-from-real-imag 9 2))

(add x z1)
