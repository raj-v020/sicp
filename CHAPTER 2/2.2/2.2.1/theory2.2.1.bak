#lang sicp

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (lengthi items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (last-pair items)
  (list-ref items (- (length items) 1)))

(define (reverse items)
  (define (reversei rlist len)
    (if (= len 0) rlist
        (reversei (append rlist
                          (list (list-ref items (- len 1))))
                  (- len 1))))
  (reversei (list) (length items)))

(define squares (list 1 4 9 16 25))
(reverse squares)

(last-pair squares)
(define odds (list 1 3 5 7))
(length odds)
(list-ref squares 2)
(car (cddr squares))