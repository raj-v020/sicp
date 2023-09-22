#lang sicp
(define (is-even? a)
  (= (remainder a 2) 0))
(define (is-odd? a)
  (not (is-even? a)))

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

(define (same-parity f . w)
  (define (create-list a test len)
      (if (= len 0)
          (append (list f) (reverse a))
          (let ((val (list-ref w (- len 1))))
            (create-list (append a (if (test val) (list val) nil)) test (- len 1)))))
  (let ((t (if (is-even? f) is-even? is-odd?)))
    (create-list nil t (length w))))

(define l1 (list 3 5 2 4))
;(list-ref l1 3)
(same-parity 2 3 4 5 6 7 9 3 0)
