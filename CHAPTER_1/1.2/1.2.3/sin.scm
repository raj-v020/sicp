#lang sicp

(define (cube x)
  (* x x x))

(define (p x)
  (- (* 3 x) (* 4 (cube x))))

(define (sine x)
  (define tolerance 0.1)
  (if (< (abs x) tolerance)
      x
      (p (sine (/ x 3)))))

(sine (/ 3.14159 2))
(sine (* 2 3.14159))
(sine 12.15)
