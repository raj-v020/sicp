#lang sicp
(define (inc a) (+ a 1))
(define (identity x) x)

(define (product term a next b)
  (define (producti m cprod)
    (if (> m b) cprod
          (producti (next m) (* (term m) cprod 1.0))))
  (producti a 1))

(define (factorial n)
  (define (fact a b)
    (product identity a inc b))
  (fact 1 n))

(define (square a) (* a a))
(define (even a) (* 2 a))
(define (odd a) (+ (even a) 1))

(define (productr term a next b)
  (if (> a b)
      1.0
      (* (term a) (productr term (next a) next b))))


(define (term1 a) (/ (even a) (odd a)))
(define (term2 a) (/ (even (+ a 1)) (odd a)))


(define (pi n)
  (* 4 (* (product term1 1 inc n) (product term2 1 inc n))))

(pi 9000000)