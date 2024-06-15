#lang sicp

(define (square x) (* x x))
(define (require p) (if (not p) (amb)))

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

(define (an-integer-between low high)
  (require (<= low high))
  (amb low (an-integer-between (+ low 1) high)))
 
(define (a-pythagorean-triple-between low high)
  (let* ((i (an-integer-between low high))
         (j (an-integer-between i high))
         (k (an-integer-between j high)))
    (require (= (+ (square i) (square j))
                (square k)))
    (list i j k)))

(define (all-pythagorean-triples)
  (let* ((k (an-integer-starting-from 1))
         (j (an-integer-between 1 k))
         (i (an-integer-between 1 j)))
    (require (= (+ (square i) (square j))
                (square k)))
    (list i j k)))
 
#|(a-pythagorean-triple-between 1 10)
(a-pythagorean-triple-between 5 20)
(a-pythagorean-triple-between 9 20)
(a-pythagorean-triple-between 39 100)|#

(all-pythagorean-triples)
