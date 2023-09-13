#lang sicp

(define (inc n) (+ n 1))
(define (identity x) x)

(define (pi-sum a b)
(define (pi-term x)
(/ 1.0 (* x (+ x 2))))
(define (pi-next x)
(+ x 4))
(sum pi-term a pi-next b))


(define (sum-integers a b)
(sum identity a inc b))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
(define (cube x) (* x x x))
(define (iseven? a) (= (remainder a 2) 0))

(define (integral f a b n)
  (define h (/ (- b a) n))
  (define (term p) (* (cond ((or (= p 0) (= p n)) 1)
                            ((iseven? p) 2)
                            (else 4)) (f (+ a (* p h)))))
  (* (/ h 3) (sum term 0 inc n)))

(integral cube 0 2 100)



