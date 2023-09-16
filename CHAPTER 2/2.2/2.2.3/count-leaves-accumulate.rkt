#lang sicp
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (count-leaves tree)
  (accumulate + 0 (map (lambda (t)
                         (if (pair? t)
                             (count-leaves t)
                             (if (null? t) 0 1))) tree)))

(count-leaves (list 1 (list 5 6) (list 7 4 8 (list 5 2) 0) 4 1 9 (list 2) -1))
(count-leaves (list 1 2 (list 2 3 (list 7) 5) 4))