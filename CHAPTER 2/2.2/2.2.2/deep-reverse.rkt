#lang sicp
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define l1 (list 1 3 (list 5 7) 9))
(define l2 (list (list 7)))
(define l3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
#|l1
(car (cdaddr l1)) (newline) 
l2
(caar l2) (newline)
l3
(cadadr (cadadr (cadr (cadr l3)))) (newline)|#
 (define (deep-reverse tree)
   (display tree)(newline)
   (cond ((null? tree) 0)
         ((not (pair? (car tree))) (cons (deep-reverse (cdr tree)) (list (car tree))))
         ((not (null? (cdr tree))) (cons (deep-reverse (cdr tree)) (list (reverse (car tree)))))
         (else (cons (deep-reverse (car (cdr tree))) (list (reverse ()))))
         )) 
(define x (list 1 2 3))
(define y (list 4 5 6))
(deep-reverse (list x y))

