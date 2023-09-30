#lang sicp

(define (power b n)
  (if (= n 0) 1 (* b (power b (- n 1)))))

(define (pair-nums a b)
  (* (power 2 a) (power 3 b)))
(define (num1 p)
  (define (iter t i)
    (if (not (= (remainder t 2) 0)) i (iter (/ t 2) (+ i 1))))
  (iter p 0))
(define (num2 p)
  (define (iter t i)
    (if (not (= (remainder t 3) 0)) i (iter (/ t 3) (+ i 1))))
  (iter p 0))
(define p1 (pair-nums 30 38))
(num1 p1)
(num2 p1)