#lang sicp

(define (square x) (* x x))

(define make-point cons)
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define make-line-seg cons)
(define (start-seg seg) (car seg))
(define (end-seg seg) (cdr seg))

(define (seg-length s)
  (sqrt (+ (square (- (x-point (end-seg s))
                      (x-point (start-seg s))))
           (square (- (y-point (end-seg s))
                      (y-point (start-seg s)))))))

(define (make-rectangle p1 p2) (cons p1 p2))

(define (rec-third-point p1 p2)
  (make-point (x-point p1) (y-point p2)))

(define (length-rec r)
  (seg-length (make-line-seg (car r) (rec-third-point (car r) (cdr r)))))
(define (breadth-rec r)
  (seg-length (make-line-seg (cdr r) (rec-third-point (car r) (cdr r)))))

(define (rec-measures r)
   (display "length = ")
   (display (length-rec r))
   (display "  breadth = ")
   (display (breadth-rec r)))

(define (perimeter-rectangle r)
  (newline)(* 2 (+ (length-rec r) (breadth-rec r))))

(define (area-rectangle r)
  (* (length-rec r) (breadth-rec r)))


(define p1 (make-point 2 3))
(define p2 (make-point 4 3))
(define p3 (make-point 2 0))

(define dp1 (make-point 2 3))
(define dp2 (make-point 4 0))

(define seg1 (make-line-seg p1 p2))
(define seg2 (make-line-seg p1 p3))

(define rec1 (make-rectangle dp1 dp2))
(rec-measures rec1)
(perimeter-rectangle rec1)
(area-rectangle rec1)