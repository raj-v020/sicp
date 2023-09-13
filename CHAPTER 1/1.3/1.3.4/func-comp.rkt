#lang sicp
(define (square x) (* x x))
(define (inc a) (+ 1 a))
(define (compose f g) (lambda (x) (f (g x))))

(define (repeated f n)
  (define (repeatedi g i)
    (if (= i n) g (repeatedi (lambda (x) ((compose f g) x)) (+ i 1))))
  (repeatedi f 1))

((repeated square 2) 2000)
((repeated inc 5) 2000)
