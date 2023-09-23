#lang sicp
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))
(define v1 (list 1 2 3))
(define v2 (list 3 4 5))
(define m1 (list (list 0 2 4)
                 (list 3 5 10)
                 (list 8 9 18)))
(define m2 (list (list 0 0 0)
                 (list 2 2 2)
                 (list -1 -1 -1)))
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (mv)
         (dot-product mv v)) m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (mv)
           (accumulate (lambda (col rest)
                         (cons (dot-product mv col) rest)) nil cols)) m)))

(matrix-*-matrix m1 m2)