#lang scheme
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (remove-element x set)
  (define (iter s removed-set bool)
    (cond (bool (append removed-set s))
          ((= (car s) x)
           (iter (cdr s) removed-set #t))
          (else (iter (cdr s) (cons (car s) removed-set) #f))))
    (iter set '() #f))

(define (adjoin-set x set)
  (cons x set))

(define (union-set set1 set2)
  (append set1 set2))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1)
                                            (remove-element (car set1) set2))))
        (else (intersection-set (cdr set1) set2))))

(intersection-set '(2 3 2 4 5 2 6) '(8 1 0 4 8 2 0 2))


