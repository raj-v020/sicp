#lang scheme

(define (make-accumulator num)
  (let ((sum num))
    (lambda (n)
      (begin (set! sum (+ sum n))
             sum))))

(define A (make-accumulator 0))
(define B (make-accumulator 5))



