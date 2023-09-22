#lang sicp
(define (abs x) (if (> x 0) x (- x)))
(define (gcd a b)
  (if (= b 0) a (gcd b (remainder a b))))
(define (make-rat a b)
  (let ((g (gcd (abs a) (abs b))))
    (if (or (and (< a 0) (< b 0)) (and (> a 0) (< b 0)))
           (cons (/ (- 0 a) g) (/ (- 0 b) g))
          (cons (/ a g) (/ b g))
          )
    ))
(define numer car)
(define denom cdr)


(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))


(print-rat (make-rat -2 -3))
