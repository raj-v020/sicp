#lang sicp
(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))
(define (count-leaves items)
  (if (null? items) 0 (+ (if (pair? (car items)) (count-leaves (car items)) 1) (count-leaves (cdr items)))))
(define l1 (cons (list 1 2) (list 3 4)))
(length l1)
(count-leaves (list l1 l1)) 
