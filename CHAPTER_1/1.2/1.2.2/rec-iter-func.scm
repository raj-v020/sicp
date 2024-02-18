#lang sicp

(define (rec-func n)
  (if (< n 3)
      n
      (+ (rec-func (- n 1)) (* 2 (rec-func (- n 2))) (* 3 (rec-func (- n 3))))))

(define (iter-func n)
  (define (iter arg a b c)
    (if (< arg 3)
        c
        (iter (- arg 1) b c (+ c (* 2 b) (* 3 a)))))
  (iter n 0 1 2))

(define n 7)
(rec-func n)
(iter-func n)
