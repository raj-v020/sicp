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

(define (sign-change-detector a b)
  (cond ((and (< (* a b) 0) (> a 0)) 1)
        ((and (< (* a b) 0) (> b 0)) -1)
        (else 0)))

(define (zero-crossings sense-data)
  (stream-map sign-change-detector
              sense-data
              (cons-stream 0 sense-data)))

(define (smooth signal)
  (stream-map (lambda (x y) (/ (+ x y) 2)) signal (cons-stream 0 signal)))

(define (make-zero-crossings input-stream)
  (define (iter signal lv)
    (cons-stream
     (sign-change-detector (stream-car signal) lv)
     (iter (stream-cdr signal) (stream-car signal))))
  (iter (smooth input-stream) 0))