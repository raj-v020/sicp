#lang sicp

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define (improved-count-pairs x)
  (let ((counted-pairs '()))
    (define (helper x) (cond ((not (pair? x)) 0)
                             ((memq x counted-pairs) 0)
                             (else (set! counted-pairs
                                         (cons x counted-pairs))
                                   (+ (helper (car x))
                                      (helper (cdr x))
                                      1))))
    (helper x)))

(define x3 '(a b c))
(define m '(a))
(define n (cons m m))
(define x7 (cons n n))
(define x4 (cons 'a n))
(define l (cons 'a m))
(define x5 (cons l l))

#|
x3
(count-pairs x3)
(improved-count-pairs x3)
x4
(count-pairs x4)
(improved-count-pairs x4)
x5
(count-pairs x5)
(improved-count-pairs x5)
x7
(count-pairs x7)
(improved-count-pairs x7)
|#
