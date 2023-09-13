#lang racket
(require racket/trace)

(define (divides? n t)
  (= (remainder n t) 0))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor a t)
    (cond ((> (* t t) a) a)
          ((divides? a t) t)
          (else (find-divisor a (+ t 1)))))

(smallest-divisor 199)
(trace find-divisor)
