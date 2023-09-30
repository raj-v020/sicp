#lang sicp
(define (nofilter a) #t)
(define (square x) (* x x))
(define (add a b) (+ a b))
(define (mul a b) (* a b))
(define (iseven? a) (= (remainder a 2) 0))
(define (smallest-divisor n) (find-divisor n 2))


(define (gcd a b)
(if (= b 0)
a
(gcd b (remainder a b))))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
(if (< n 2) #f (= n (smallest-divisor n))))

(define (filtered-accumulate filter combiner null-value term a next b)
  (define (accumulatei accumulation m)
    (cond ((> m b) accumulation)
          ((filter m) (accumulatei (combiner (term m) accumulation) (next m)))
          (else (accumulatei  accumulation (next m)))))
  (accumulatei null-value a))
 
(define (sum-of-prime-squares a b)
  (filtered-accumulate prime? add 0 square a inc b))

(define (sum-of-even-squares a b)
  (filtered-accumulate iseven? add 0 square a inc b))

(define (sum a b)
  (filtered-accumulate nofilter add 0 identity a inc b))

(define (prod-relprimes n)
  (define (relprime? a) (= (gcd a n) 1))
  (filtered-accumulate relprime? mul 1 identity 1 inc n))

(sum 1 10)
(prod-relprimes 10)