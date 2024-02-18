#lang sicp

(define (square x) (* x x))
(define (sum-squares a b) (+ (square a)
                             (square b)))

(define (sum-square-largest x y z)
  (cond ((and (< z x) (< z y)) (sum-squares x y))
        ((and (< y x) (< y z)) (sum-squares x z))
        (else (sum-squares y z))))

(sum-square-largest 3 2 5)
