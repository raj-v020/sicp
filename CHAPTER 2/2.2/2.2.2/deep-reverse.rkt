#lang sicp

(define l1 (list 1 3 (list 5 7) 9))
(define l2 (list (list 7)))
(define l3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(define (last-pair items)
  (list (list-ref items (- (length items) 1))))

(define l (list 1 (list 0 1 2) 3 (list 0 3) (list 0 2) 4 5))

(define (deep-reverse items)
  (if (pair? items)
      (append (deep-reverse (cdr items)) (list (deep-reverse (car items))))
      items
        ))

(define (deep-reverse-iter items)
  (define (iter list res)
    (cond ((null? list) res)
          ((not (pair? (car list))) (iter (cdr list) (cons (car list) res)))
          (else (iter (cdr list) (cons (deep-reverse (car list)) res)))
           ))
  (iter items '()))
(define x (list 1 2 3))
(define y (list 4 5 6))
l
(deep-reverse l)
(deep-reverse-iter l)

