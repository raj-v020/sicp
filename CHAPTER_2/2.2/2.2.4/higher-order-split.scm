#lang sicp
(define (split p1 p2)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split p1 p2) (- n 1))))
          (p1 painter (p2 smaller smaller))))))