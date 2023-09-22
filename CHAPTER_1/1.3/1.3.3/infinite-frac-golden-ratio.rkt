#lang sicp
(define (term i) 1.0)

(define (cont-frac n d k)
  (define (cont-frac-iter res m)
    (if (= m 0) res (cont-frac-iter (/ (n m) (+ (d m) res)) (- m 1))))
  (cont-frac-iter (/ (n k) (d k)) (- k 1)))

(define (cont-frac-recur n d k)
  (define (cont-frac-r i)
    (if (= i k) (/ (n i) (d i)) (/ (n i) (+ (d i) (cont-frac-r (+ i 1))))))
  (cont-frac-r 1))

(display (/ 1 (cont-frac term term 38)))