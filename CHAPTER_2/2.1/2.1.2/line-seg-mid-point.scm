#lang sicp

(define (square x) (* x x))

(define make-point cons)
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")"))

(define (add-points m n)
  (make-point (+ (x-point m) (x-point n)) (+ (y-point m) (y-point n))))
(define (mul-num-to-point k p)
  (make-point (* k (x-point p)) (* k (y-point p))))

(define p1 (make-point 2 3))
(define p2 (make-point 4 2))

(define make-line-seg cons)
(define (start-seg seg) (car seg))
(define (end-seg seg) (cdr seg))

(define seg1 (make-line-seg p1 p2))

(define (seg-length s)
  (sqrt (+ (square (- (x-point (end-seg s))
                      (x-point (start-seg s))))
           (square (- (y-point (end-seg s))
                      (y-point (start-seg s)))))))
(seg-length seg1)

(define (seg-mid-point s)
  (print-point
   (mul-num-to-point 0.5 (add-points (start-seg s) (end-seg s)))))

(seg-mid-point (make-line-seg p1 p2))
