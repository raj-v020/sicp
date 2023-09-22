#lang sicp
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x)
                            (append x (list (car s)))) rest)))))

(define l (list 2 3 4))
(subsets l)
