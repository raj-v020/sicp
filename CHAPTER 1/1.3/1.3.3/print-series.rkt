#lang sicp
(define (inc c) (+ c 1))

(define (d-euler i)
  (if (= (modulo i 4) 1)
      9
      1))

(define (display-serie f n)
  (define (rec i)
    (display (f i)) (display "  ")
    (if (= i n)
        (newline)
        (rec (inc i))))
  (rec 1))

(display-serie d-euler 12)