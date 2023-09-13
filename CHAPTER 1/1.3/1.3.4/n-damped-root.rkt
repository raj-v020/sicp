#lang sicp
(define (square x) (* x x))
(define (pow x n)
  (define (powi i res)
    (if (= i n) res (powi (+ i 1) (* res x))))
  (powi 0 1))
(define (avg a b) (/ (+ a b) 2))
(define tolerance 0.00000000000001)
(define (average-damp f)
(lambda (x) (avg x (f x))))

(define (compose f g) (lambda (x) (f (g x))))

(define (repeated f n)
  (define (repeatedi g i)
    (if (= i n) g (repeatedi (lambda (x) ((compose f g) x)) (+ i 1))))
  (repeatedi f 1))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess i)
    (display i) (display ") ") (display guess) (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next 
          (try next (+ i 1)))))
  (try first-guess 1))

(define (nth-root-damped x nth damp)
  (fixed-point
    ((repeated average-damp damp)
    (lambda (y)
      (/ x (pow y (- nth 1)))))
   1.0))