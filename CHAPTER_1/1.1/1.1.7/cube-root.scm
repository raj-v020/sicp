#lang sicp

(define (average x y z)
  (/ (+ x y z) 3))

(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

(define (improve guess x)
  (average guess guess (/ x (square guess))))

(define (good-enough? guess x)
  (= (improve guess x) guess))

(define (cube-root-iter guess x)
  (if (good-enough? guess x)
      guess
      (cube-root-iter (improve guess x) x)))

(define (cube-root x)
  (cube-root-iter 1.0 x))

(cube 0.25)
(cube-root 0.015625)
