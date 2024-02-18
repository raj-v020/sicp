#lang sicp

(define (double x)
  (+ x x))

(define (halve x)
  (/ x 2))

(define (mul a b)
  (cond ((= b 0) 0)
        ((even? b) (mul (double a) (halve b)))
        (else (+ a (mul a (- b 1))))))

(define (mul-iter a b)
  (define (iter aux-a aux-b prod)
  (cond ((= aux-b 0) prod)
        ((even? aux-b) (iter (double aux-a) (halve aux-b) prod))
        (else (iter aux-a (- aux-b 1) (+ prod aux-a)))))
  (iter a b 0))

(mul 3 200)
(mul-iter 3 200)
