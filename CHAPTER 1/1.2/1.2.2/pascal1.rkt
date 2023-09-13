#lang sicp
(define (pascal m n) (cond ((or (= n 1)(= m 2)) 1)
                           ((= n m) 1)
                           (else (+ (pascal (- m 1) (- n 1))(pascal (- m 1) n)))))
(define (layer m)
  (define (iter n)
    (display (pascal m n)) (display "  ")
    (if (= n m) (newline)
        (iter (+ n 1)))
    )    
  (iter 1))

(define (display-pascal a)
  (define (iter n)
    (layer n)
    (if (> n a) (newline) (iter (+ n 1))))
  (iter 1))

(display-pascal 10)