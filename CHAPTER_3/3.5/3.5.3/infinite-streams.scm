#lang sicp

(define (average a b) (/ (+ a b) 2))
(define (square x) (* x x))
(define stream-car car)
(define (stream-cdr s) (force (cdr s)))

(define (display-line x) (display x) (newline))

(define (display-stream s n)
  (if (zero? n)
      (display-line "done")
      (begin
        (display-line (stream-car s))
        (display-stream (stream-cdr s) (- n 1) ))))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map f . ss)
  (if (stream-null? (car ss))
      the-empty-stream
      (cons-stream (apply f (map stream-car ss))
                   (apply stream-map f (map stream-cdr ss)))))

(define (add-streams s1 s2) (stream-map + s1 s2))

(define (partial-sums stream)
  (if (stream-null? stream)
      the-empty-stream
      (cons-stream
       (stream-car stream)
       (add-streams (stream-cdr stream) (partial-sums stream)))))

(define (sqrt-stream x)
  (define guesses
    (cons-stream
     1.0
     (stream-map (lambda (guess) (sqrt-improve guess x))
                 guesses)))
  guesses)

(define (stream-limit stream tol)
  (define (iter prev s)
    (cond ((stream-null? s) 0)
          ((< (abs (- (stream-car s) prev)) tol) (stream-car s))
          (else (iter (stream-car s) (stream-cdr s)))))
  (iter (stream-car stream) (stream-cdr stream)))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0)) ; Sn 1
        (s1 (stream-ref s 1)) ; Sn
        (s2 (stream-ref s 2))) ; Sn+1
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(define (log2-summands n)
  (cons-stream (/ 1 n) (stream-map - (log2-summands (+ n 1)))))

(define log2-stream
  (partial-sums (log2-summands 1.0)))

(define (make-tableau transform s)
  (cons-stream s (make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))

(display-stream (accelerated-sequence euler-transform log2-stream) 10)