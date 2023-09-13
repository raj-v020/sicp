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

(define (no-more? coin-values)
  (= (length coin-values) 0))

(define (count-change amount) (cc amount us-coins))

(define (cc amount currency)
    (cond ((= amount 0) 1)
          ((or (< amount 0) (no-more? currency)) 0)
          (else (+ (cc amount
                       (except-first-denomination currency))
                   (cc (- amount
                          (first-denomination
                           currency))
                       currency)))))

(define (except-first-denomination currency)
  (cdr currency))

(define (first-denomination coin-values)
  (car coin-values))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(define in-coins (list 2000 500 200 100 50 20 10 5 2 1))
(cc 5 in-coins)