#lang sicp

(define (display-line x) (display x) (newline))
;(define (delay x) (lambda () x))
;(define (force x) (x))

(define (cons-stream a b) (cons a (delay b)))
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (display-stream s)
  (stream-for-each display-line s))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

#|(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))|#

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter
                       pred
                       (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

#|(define (show x)
  (display-line x)
  x)

(define x
  (stream-map show
              (stream-enumerate-interval 0 10))) (newline)
(stream-ref x 5) (newline)
(stream-ref x 7)|#

(define sum 0) (display-line sum)
(define (accum x) (set! sum (+ x sum)) sum) (display-line sum)
(define seq
  (stream-map accum
              (stream-enumerate-interval 1 20))) (display-line sum)

(define y (stream-filter even? seq)) (display-line sum) (display-line sum)
(define z
  (stream-filter (lambda (x) (= (remainder x 5) 0))
                 seq)) (display-line sum)

(stream-ref y 7)
(display-stream z)