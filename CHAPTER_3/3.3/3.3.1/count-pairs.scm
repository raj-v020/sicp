#lang sicp

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define x3 '(a b c))
(define m '(a))
(define n (cons m m))
(define x7 (cons n n))
(define x4 (cons 'a n))
(define l (cons 'a m))
(define x5 (cons l l))
