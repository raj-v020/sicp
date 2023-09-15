#lang sicp
(define (map-tree proc tree)
  (if (= (length tree) 0)
      nil
      (cons (proc (car tree)) (map-tree proc (cdr tree)))))

(define (scale-tree tree factor)
  (map-tree (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))
(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)