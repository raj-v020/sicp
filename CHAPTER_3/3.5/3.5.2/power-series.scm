#lang sicp

(define (display-line x) (display x) (newline))

(define ones (cons-stream 1 ones))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (display-stream s n)
  (if (zero? n)
      (display-line "done")
      (begin
        (display-line (stream-car s))
        (display-stream (stream-cdr s) (- n 1) ))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))

(define (add-streams s1 s2) (stream-map + s1 s2))
(define (mul-streams s1 s2) (stream-map * s1 s2))

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1)
                  (stream-car s2))
               (add-streams (scale-stream (stream-cdr s1) (stream-car s2))
                            (mul-series s1 (stream-cdr s2)))))
(define (negate-stream s)
  (stream-map - s))

(define integers
  (cons-stream 1 (add-streams ones integers)))

(define (integrate-series stream)
  (stream-map (lambda (a b) (/ b a)) integers stream))

(define (invert-unit-series s)
  (if (= (stream-car s) 1)
      (cons-stream 1
                   (negate-stream
                    (mul-series (stream-cdr s)
                                (invert-unit-series s))))
      (error "Not a unit series: INVERT-UNIT-SERIES" s)))

;s1/s2
; (a1 + s1r*x)/s2
; a1/s2 + (s1r/s2)*x
; a1*s2i + (s1r/s2)*x

(define (div-series s1 s2)
  (if (not (= (stream-car s2) 0))
      (let ((s1* (scale-stream s1 (/ 1 (stream-car s2))))
            (s2* (scale-stream s2 (/ 1 (stream-car s2)))))
        (add-streams (scale-stream (invert-unit-series s2*) (stream-car s1*))
                     (cons-stream 0 (div-series (stream-cdr s1*)
                                                s2*))))
      (error "Denominator has a zero constant term: DIV-SERIES" s2)))

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define cosine-series (cons-stream 1 (negate-stream (integrate-series sine-series))))
(define sine-series (cons-stream 0 (integrate-series cosine-series)))

#|(display-stream
 (add-streams (mul-series sine-series sine-series)
              (mul-series cosine-series cosine-series))
 10)|#

(display-stream (div-series sine-series cosine-series) 10)
