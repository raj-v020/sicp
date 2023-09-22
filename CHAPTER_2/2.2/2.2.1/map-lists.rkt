#lang sicp
(define (square x) (* x x))
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))
(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))
(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (map-list proc items)
  (if (= (length items) 0) nil (cons (proc (car items)) (map-list proc (cdr items)))))

(define (for-each proc items)
  (if (= (length items) 0) (display " ") (and (proc (car items)) (for-each proc (cdr items)))))
 
(define (reverse items)
  (define (reversei rlist len)
    (if (= len 0) rlist
        (reversei (append rlist
                          (list (list-ref items (- len 1))))
                  (- len 1))))
  (reversei (list) (length items)))

#| (define (square-list list)
  (if (= (length list) 0) nil
      (cons (square (car list)) (square-list (cdr list))))) |#

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        (reverse answer)
        (iter (cdr things)
              (cons (square (car things)) answer))))
  (iter items nil))
(define l1 (list 1 2 3 4 5))
(square-list l1)
(map square l1)
(for-each (lambda (x)
(newline)
(display x))
(list 57 321 88))