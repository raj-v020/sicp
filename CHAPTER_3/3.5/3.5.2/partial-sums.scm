#lang sicp

(define (display-line x) (display x) (newline))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define ones (cons-stream 1 ones))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (add-streams s1 s2) (stream-map + s1 s2))
(define (mul-streams s1 s2) (stream-map * s1 s2))

(define integers
  (cons-stream 1 (add-streams ones integers)))

(define (partial-sums stream)
  (if (stream-null? stream)
      the-empty-stream
      (cons-stream
       (stream-car stream)
       (add-streams (stream-cdr stream) (partial-sums stream)))))

(define partial-integers (partial-sums integers))
(stream-ref partial-integers 3)
