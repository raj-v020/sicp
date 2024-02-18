#lang sicp
(define (square x) (* x x))
(define (mulb b c) (* b c))
(define (expt b n)
  (define (expti p c)
    (cond ((= c 0) 1)
          ((= c 1) p)
          ((even? n) (expt (square p) (/ c 2)))
          (else (expti (* b p) (- c 1)))))
    (expti b n))

(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (iexpt b n)
  (define (invar-expt p q a)
    (cond ((= q 0) a)
          ((even? q) (invar-expt (square p) (/ q 2) a))
          (else (invar-expt p (- q 1) (* p a)))))
  (invar-expt b n 1))

  

(fast-expt 2 10)
(iexpt 2 10)
