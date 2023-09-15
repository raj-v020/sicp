#lang sicp
(define (square x) (* x x))
(define (map-tree proc tree)
  (if (= (length tree) 0)
      nil
      (cons (proc (car tree)) (map-tree proc (cdr tree)))))

(define (tree-map proc tree)
  (if (= (length tree) 0)
      nil
      (cons (if (list? (car tree))
                (tree-map proc (car tree))
                (proc (car tree))) (tree-map proc (cdr tree)))))

(define (scale-tree tree factor)
  (map-tree (lambda (sub-tree)
              (if (pair? sub-tree)
                  (scale-tree sub-tree factor)
                  (* sub-tree factor)))
            tree))
(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)

(define (square-t tree)
  (cond ((null? tree) nil)
        ((list? (car tree)) (cons (square-t (car tree)) (square-t (cdr tree))))
        (else (cons (square (car tree)) (square-tree (cdr tree))))))

(define (square-tree tree)
  (map-tree (lambda (x)
         (if (pair? x)
             (square-tree x)
             (* x x)))
       tree))
(define (tree-square tree)
  (tree-map square tree))

(square-tree (list 2 9 (list 4 (list 8 5) 6) 3 (list 1 0) 7))
(square-t (list 2 9 (list 4 (list 8 5) 6) 3 (list 1 0) 7))
(tree-square (list 2 9 (list 4 (list 8 5) 6) 3 (list 1 0) 7))