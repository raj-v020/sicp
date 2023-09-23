#lang sicp
(define dx 0.00001)
(define (avg3 a b c) (/ (+ a b c) 3))
(define (square x) (* x x))

(define (compose f g) (lambda (x) (f (g x))))

(define (repeated f n)
  (define (repeatedi g i)
    (if (= i n) g (repeatedi (lambda (x) ((compose f g) x)) (+ i 1))))
  (repeatedi f 1))

(define (smooth f) (lambda (x) (avg3 (f (- x dx)) (f x) (f (+ x dx)))))
(define (n-fold-smooth f n) ((repeated smooth n) f))


((smooth square) 2)
((n-fold-smooth square 7) 2)