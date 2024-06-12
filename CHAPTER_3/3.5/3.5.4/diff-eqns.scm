#lang sicp

(define (cons-stream a b) (cons a (delay b)))
(define stream-car car)
(define (stream-cdr s) (force (cdr s)))
(define stream-null? null?)
(define the-empty-stream '())

(define (stream-map f . ss)
  (if (stream-null? (car ss))
      the-empty-stream
      (cons-stream (apply f (map stream-car ss))
                   (apply stream-map f (map stream-cdr ss)))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (add-streams s1 s2) (stream-map + s1 s2))

(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream
     initial-value
     (let ((integrand (force delayed-integrand)))
       (add-streams (scale-stream integrand dt) int))))
  int)

(define (delayed-integral delayed-integrand initial-value dt)
  (cons-stream
   initial-value
   (let ((integrand (force delayed-integrand)))
     (if (stream-null? integrand)
         the-empty-stream
         (integral (delay (stream-cdr integrand))
                   (+ (* dt (stream-car integrand))
                      initial-value)
                   dt)))))

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(define (solve-2nd f dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)

(define (RLC R L C dt)
  (lambda (vc0 il0)
    (define vc (integral (delay dvc) vc0 dt))
    (define il (integral (delay dil) il0 dt))
    (define dil (add-streams (scale-stream vc (/ 1 L))
                             (scale-stream il (/ (- R) L))))
    (define dvc (scale-stream il (/ -1 C)))
    (cons vc il)))
