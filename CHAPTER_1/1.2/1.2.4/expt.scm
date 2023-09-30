#lang sicp
(define (square x) (* x x))
(define (mulb b c) (* b c))
(define (expt b n)
  (define (expti p c)
    (cond ((= c 0) 1)
          ((= c 1) p)
          ((even? n) (expt (square p) (/ c 2)))
          (else (expti (* b p) (- c 1)))))
    (expti b n))

(define (even? n)
(= (remainder n 2) 0))

(expt 2 10)