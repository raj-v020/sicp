#lang sicp
(define (identity x) x)
(define (inc a) (+ a 1))
(define (add a b) (+ a b))
(define (mul a b) (* a b))

(define (accumulate combiner null-value term a next b)
  (define (accumulatei accumulation m)
    (if (> m b)
      accumulation
      (accumulatei (combiner (term m) accumulation) (next m))))
  (accumulatei null-value a))

(define (sum a b) 
  (accumulate add 0 identity a inc b))

(define (product a b)
  (accumulate mul 1 identity a inc b))

(sum 1 100)
(product 1 3)