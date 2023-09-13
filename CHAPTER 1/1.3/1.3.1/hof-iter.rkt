#lang sicp
(define (cube x) (* x x x))
(define (iseven? a) (= (remainder a 2) 0))
(define (inc n) (+ n 1))
(define (identity x) x)

(define (sum term a next b)
  (define (sum-iter csum m)
    (if (> m b)
        csum
        (sum-iter (+ csum (term m)) (next m))))
  (sum-iter 0 a))

(define (sum-integers a b)
(sum identity a inc b))

(define (integral f a b n)
  (define h (/ (- b a) n))
  (define (term p) (* (cond ((or (= p 0) (= p n)) 1)
                            ((iseven? p) 2)
                            (else 4)) (f (+ a (* p h)))))
  (* (/ h 3) (sum term 0 inc n)))

(integral cube 0 1 100)