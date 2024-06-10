#lang sicp

(define (display-line x) (display x) (newline))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define ones (cons-stream 1 ones))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (add-streams s1 s2) (stream-map + s1 s2))
(define (mul-streams s1 s2) (stream-map * s1 s2))

(define integers
  (cons-stream 1 (add-streams ones integers)))

(define factorials
  (cons-stream 1 (mul-streams integers factorials)))

#|
(1 (delay (mul-streams integers factorials)))
(1 (stream-map * integers factorials))
(1 1 (delay (apply stream-map (cons proc (map stream-cdr (integers factorials))))))
(1 1 (stream-map proc (stream-cdr integers) (stream-cdr factorials)))
(1 1 2 (delay (apply stream-map (cons proc (map stream-cddr (integers factorials))))))
(1 1 2 (stream-map proc (stream-cddr integers) (stream-cddr factorials)))
(1 1 2 6 ...
|#
(stream-ref factorials 4)
