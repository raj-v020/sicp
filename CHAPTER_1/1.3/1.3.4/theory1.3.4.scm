#lang sicp
(define dx 0.00001)
(define tolerance 0.00001)

(define (square x) (* x x))
(define (average a b) (/ (+ a b) 2))
(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

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

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))
(define (sqrt x)
  (newtons-method
   (lambda (y) (- (square y) x)) 1.0))

(define (cubic a b c) (lambda (x) (+ (* x x x) (* a (* x x)) (* b x) c)))
(newtons-method (cubic 1 1 1) 1.0)