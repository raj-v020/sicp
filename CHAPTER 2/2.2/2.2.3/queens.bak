#lang sicp
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define empty-board (list (list 0 0 0 0 0 0 0 0)
                          (list 0 0 0 0 0 0 0 0)
                          (list 0 0 0 0 0 0 0 0)
                          (list 0 0 0 0 0 0 0 0)
                          (list 0 0 0 0 0 0 0 0)
                          (list 0 0 0 0 0 0 0 0)
                          (list 0 0 0 0 0 0 0 0)
                          (list 0 0 0 0 0 0 0 0)))

#|(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                    new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))|#
(define eg-board (list (list 0 0 0 0 0 0 0 0)
                       (list 0 0 0 0 0 0 0 0)
                       (list 0 0 0 0 0 0 0 0)
                       (list 0 0 0 0 0 0 0 0)
                       (list 0 0 0 0 0 0 0 0)
                       (list 0 0 0 0 0 0 0 0)
                       (list 0 0 0 0 0 0 0 0)
                       (list 0 0 0 0 0 0 0 0)))
(define (adjoin-position new-row k board)
  (define (iter m r)
    (cond ((> m k) (append (list r) (cdr board)))
          ((= m new-row) (iter (+ m 1) (append r (list 1))))
          (else (iter (+ m 1) (append r (list 0))))))
  (iter 1 nil))

(define (empty-list l)
  (define (iter list bool)
    (cond ((or (null? list) (not bool)) bool)
          (else (iter (cdr list) (if (= 1 (car list)) #f bool)))))
  (iter l #t))
