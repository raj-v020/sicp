#lang sicp
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
              0
              (reverse coefficient-sequence)))
(horner-eval 3 (list 1 0 5 0 3 1))
;x^2 + 2x + 3 = 9 + 6 + 3
