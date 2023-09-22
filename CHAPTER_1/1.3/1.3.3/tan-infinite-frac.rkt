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



(define (cont-frac n d k)
  (define (cont-frac-iter res m)
    (if (= m 0) res (cont-frac-iter (/ (n m) (+ (d m) res)) (- m 1))))
  (cont-frac-iter (/ (n k) (d k)) (- k 1)))
(define (tan x k) (cont-frac (lambda (y)
                               (if (= y 1) x (- (* x x)))) (lambda (z)
                                                         (- (* 2 z) 1 )) k))
(tan (/ 3.14159265359 6) 100000)
