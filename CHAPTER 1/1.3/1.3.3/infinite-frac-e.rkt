#lang sicp
(define (divisibleby3? a) (= (remainder a 3) 0))
(define (nf i) 1.0)

(define (d-euler i)
  (display (modulo i 3)) (newline)
  (if (= (modulo i 3) 2)
      (* 2(/ (+ i 1) 3))
      1))

(define (df i) (cond ((divisibleby3? (- i 2)) (* 2 (+ 1 (/ (- i 2) 3))))
                     (else 1)))

(define (cont-frac n d k)
  (define (cont-frac-iter res m)
    (if (= m 0) res (cont-frac-iter (/ (n m) (+ (d m) res)) (- m 1))))
  (cont-frac-iter (/ (n k) (d k)) (- k 1)))

(define (cont-frac-recur n d k)
  (define (cont-frac-r i)
    (if (= i k) (/ (n i) (d i)) (/ (n i) (+ (d i) (cont-frac-r (+ i 1))))))
  (cont-frac-r 1))

(+ 2 (cont-frac nf d-euler 100))