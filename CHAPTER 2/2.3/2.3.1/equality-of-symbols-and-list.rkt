#lang scheme
(define (equal? item1 item2)
  (if (and (pair? item1)
           (pair? item2))
      (and (equal? (car item1) (car item2))
           (equal? (cdr item1) (cdr item2)))
      (eq? item1 item2)))

(equal? '(this is a list) '(this (is a) list))