#lang sicp

(define (average a b) (/ (+ a b) 2))

(define (close-enough? x y) (< (abs (- x y)) 0.00000001))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))


(define tolerance 0.00000000000001)
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

(define (f x) (+ (sin x) (cos x)))

#|(define fp (fixed-point f 2.3))
(define fpv (f fp))
(display fp) (newline)
(display fpv) (newline)
(abs (- fp fpv))

(define (sqrt x) (fixed-point (lambda (y) (average y (/ x y))) (/ x 2)))|#

(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)
(fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2.0)


