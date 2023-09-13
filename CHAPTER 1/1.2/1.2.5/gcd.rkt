#lang sicp
(define (gcd a b i)
  (display i) (newline)
  (if (= b 0) a (gcd b (remainder a b) (+ i 1))))
(gcd 206 40 0)