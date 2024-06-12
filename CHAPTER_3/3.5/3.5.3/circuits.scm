#lang sicp

(define stream-car car)
(define (stream-cdr s) (force (cdr s)))

(define (stream-map f . ss)
  (if (stream-null? (car ss))
      the-empty-stream
      (cons-stream (apply f (map stream-car ss))
                   (apply stream-map f (map stream-cdr ss)))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))

(define (add-streams s1 s2) (stream-map + s1 s2))

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

(define (RC R C dt)
  (lambda (i v0)
    (add-streams (scale-stream i R)
                 (integral (scale-stream i (/ 1 C)) v0 dt))))

(define RC1 (RC 5 1 0.5))
