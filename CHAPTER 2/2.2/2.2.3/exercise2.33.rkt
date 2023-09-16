#lang sicp
(define (square x) (* x x))
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (append-s seq1 seq2)
  (accumulate cons seq2 seq1))

(define (map-s p sequence)
  (accumulate (lambda (x y)
                (cons (p x) y)) nil sequence))

(define (length-s sequence)
  (accumulate (lambda (x y)
                (+ 1 y)) 0 sequence))


(define l1 (list 2 3 5))
(define l2 (list (list 7 4) 9 8 5))
(length-s l2)