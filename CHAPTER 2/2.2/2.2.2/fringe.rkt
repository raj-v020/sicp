#lang sicp
(define l1 (list 1 3 (list 5 7) 9))
(define l2 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(define l (list 1 (list 0 1 2) 3 (list 0 3) (list 0 2) 4 5))
(define x (list 1 2 3 4))
(define y (list 4 5 6))

(define (fringe items)
  (if (pair? items) (append (if (pair? (car items))
                                (fringe (car items))
                                (list (car items)))
                            (fringe (cdr items)))
      items))
(fringe (list 1 (list 2 (list 3 4)) 5))