#lang sicp
(define (fac n)
  (define (fi p c)
    (if (< c (+ n 1)) (fi (* p c) (+ c 1)) p))
  (fi 1 1))

(define (coeff n r) (/ (fac n) (* (fac (- n r)) (fac r))))
(define (layer n)
  (define (iter r)
          (display (coeff (- n 1) r)) (display "  ")
                   (if (= r (- n 1))
                        (newline)
                        (iter (+ r 1))))
  (iter 0))


(define (display-pascal n)
  (define (iter i)
    (display (layer i))
    (if (= i n)
        (newline)
        (iter (+ i 1))))
  (iter 1))

(display-pascal 10)