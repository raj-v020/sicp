#lang sicp

(define (divides? n t)
  (= (remainder n t) 0))

(define (square x)
  (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (modified-expmod base exp m)
  (remainder (fast-expt base exp) m))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor a t)
  (define (next n)
    (if (= n 2)
        3
        (+ n 2)))
  
  (cond ((> (* t t) a) a)
        ((divides? a t) t)
        (else (find-divisor a (next t)))))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 10)
      (report-prime (- (runtime) start-time))
      #f))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes a b)
  (cond ((> a b) (newline))
        ((odd? a) (timed-prime-test a)
                  (search-for-primes (+ a 2) b))
        (else (search-for-primes (+ a 1) b))))

(define (carmichael-test n)
  (define (iter a n)
    (cond ((= a n) #t)
          ((= (expmod a n n) a) (iter (+ a 1) n))
          (else #f)))
  (iter 1 n))

(prime? 7)
(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)
(search-for-primes 1000 1019) #| 1009, 1013, 1019 |#
(search-for-primes 10000 10037) #| 10007, 10009, 10037 |#
(search-for-primes 100000 100043) #| 100003, 100019, 100043 |#
(search-for-primes 1000000 1000037) #| 1000003, 1000033, 1000037 |#

(carmichael-test 561)
(carmichael-test 1105)
(carmichael-test 1729)
(carmichael-test 2465)
(carmichael-test 2821)
(carmichael-test 6601)
(carmichael-test 10)
