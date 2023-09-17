#lang sicp
(define (list-ref items n)
  (if (or (null? (car items)) (= n 0))
      (car items)
      (list-ref (cdr items) (- n 1))))

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

(define (empty-board board-size)
  (define (iter2 len mt-list)
    (if (= len 0) mt-list
        (iter2 (- len 1) (cons 0 mt-list))))
  (define (iter1 size mt-board)
    (if (= size 0) mt-board
        (iter1 (- size 1) (cons (iter2 board-size nil) mt-board))))
  (iter1 board-size nil))

(define eg-board (list (list 0 1 0 0 0 0 0 0)
                       (list 1 0 0 0 0 0 0 0)
                       (list 0 0 1 0 0 0 0 0)
                       (list 0 0 0 0 0 0 0 0)
                       (list 0 0 0 0 0 0 0 0)
                       (list 0 0 0 0 0 0 0 0)
                       (list 0 0 0 0 0 0 0 0)
                       (list 0 0 0 0 0 0 0 0)))

(define (append-list l val pos)
  (define (appendi res lis count)
    (cond ((null? lis) res)
          ((= count pos) (appendi (append res (list val)) (cdr lis) (+ count 1)))
          (else (appendi (append res (list (car lis))) (cdr lis) (+ count 1)))))
  (appendi nil l 0))

(define (adjoin-position new-row k board)
  (define (iter m r)
    (cond ((> m (- (length board) 1)) (append-list board r (- k 1)))
          ((= m (- new-row 1)) (iter (+ m 1) (append r (list 1))))
          (else (iter (+ m 1) (append r (list 0))))))
  (iter 0 nil))

(define (row-pos lis)
  (define (iter l p)
    (if (or (null? l) (= (car l) 1))
        p
        (iter (cdr l) (+ p 1))))
  (iter lis 0))

(define (safe? k board)
  (define (row-check b rp)
    ;(if #t (display (abs (- (length board) (length b)))) 0) (newline)
    (cond ((null? b) #t)
          ((= (abs (- (length board) (length b))) (- k 1)) #t)
          ((= (list-ref (car b) rp) 1) #f)
          (else (row-check (cdr b) rp))))
  
  (define (diagonal-check rp)
    (define (iter m n b val)
     ;(display "m: ") (display m) (newline)
      ;(display "n: ") (display n) (newline)
      (cond ((= (- (length board) (length b)) (- k 1)) val)
            ((not val) val)
            ((and (< m 0) (> n (- (length board) 1)))
             (iter (+ m 1) (- n 1) (cdr b) val))
            ((< m 0) (iter (+ m 1) (- n 1) (cdr b)
                           (if (= (list-ref (car b) n) 1) #f #t)))
            ((> n (- (length board) 1)) (iter (+ m 1) (- n 1) (cdr b)
                                              (if (= (list-ref (car b) m) 1) #f #t)))
            (else (iter (+ m 1) (- n 1) (cdr b)
                        (if (or (= (list-ref (car b) n) 1)
                                (= (list-ref (car b) m) 1)) #f #t)))))
    (iter (- rp k (- 1))
          (+ rp k (- 1)) board #t))
  
  (let ((row-p (row-pos (list-ref board (- k 1)))))
    (and (diagonal-check row-p) (row-check board row-p))))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list (empty-board board-size))
        (filter
         (lambda (positions)
           (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                    new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))


(define (all-queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list (empty-board board-size))
        (flatmap
         (lambda (rest-of-queens)
           (map (lambda (new-row)
                  (adjoin-position
                   new-row k rest-of-queens))
                (enumerate-interval 1 board-size)))
         (queen-cols (- k 1)))))
  (queen-cols board-size))


(define board (list (list 0 0 0 0 0 1 0 0)
                    (list 0 0 1 0 0 0 0 0)
                    (list 1 0 0 0 0 0 0 0)
                    (list 0 0 0 0 0 0 1 0)
                    (list 0 0 0 0 1 0 0 0)
                    (list 0 0 0 0 0 0 0 1)
                    (list 0 1 0 0 0 0 0 0)
                    (list 0 0 0 1 0 0 0 0)))

(length (queens 8))