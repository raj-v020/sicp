#lang sicp

(define (opposite-pairs? a b)
  (if (> a 0) (< b 0) (> b 0)))
(define (positive-pairs? a b)
  (if (opposite-pairs? a b) #f (> a 0)))
(define (negative-pairs? a b)
  (if (opposite-pairs? a b) #f (< a 0)))

(define (make-interval a b)
  (cons (min a b) (max a b)))
(define (lower-bound i)
  (car i))
(define (upper-bound i)
  (cdr i))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (o-mul-interval x y)
  (let ((lx (lower-bound x))
        (ux (upper-bound x))
        (ly (lower-bound y))
        (uy (upper-bound y)))
    (let ((p1 (* lx ly))
          (p2 (* lx uy))
          (p3 (* ux ly))
          (p4 (* ux uy)))
      (make-interval (min p1 p2 p3 p4)
                     (max p1 p2 p3 p4)))))

(define (mul-interval x y)
  (let ((lx (lower-bound x))
        (ux (upper-bound x))
        (ly (lower-bound y))
        (uy (upper-bound y)))
    (cond ((positive-pairs? lx ux) (cond ((positive-pairs? ly uy) (make-interval (* lx ly) (* ux uy)))
                                         ((negative-pairs? ly uy) (make-interval (* ux ly) (* lx uy)))
                                         (else (make-interval (* ux ly) (* ux uy)))))
          
          ((negative-pairs? lx ux) (cond ((positive-pairs? ly uy) (make-interval (* lx uy) (* ux ly)))
                                         ((negative-pairs? ly uy) (make-interval (* ux uy) (* lx ly)))
                                         (else (make-interval (* lx uy) (* lx ly)))))
          
          (else (cond ((positive-pairs? ly uy) (make-interval (* lx uy) (* ux uy)))
                      ((negative-pairs? ly uy) (make-interval (* ux ly) (* lx ly)))
                      (else (make-interval (min (* lx uy) (* ux ly)) (max (* lx ly) (* ux uy)))))))))

(define (div-interval x y)
  (if (= (upper-bound y) (lower-bound y)) (error "DIVISION by ZERO interval")
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y))
                      (/ 1.0 (lower-bound y))))))

(define (sub-interval x y)
  (make-interval (- (lower-bound y) (lower-bound x))
                 (- (upper-bound y) (upper-bound x))))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2.0))
(define (center i)
  (+ (lower-bound i) (width i)))
(define (percent i)
  (abs (/ (* 100 (width i)) (center i))))
(define (show-percent i)
  (display (percent i)) (display "%"))

(define i1 (make-interval 1 2))
(define i2 (make-interval 2 3))
(define i3 (make-interval 1 4))

(define addi (add-interval i1 i2))
(define subi (sub-interval i1 i2))
(define muli (mul-interval i1 i2))
(define divi (div-interval i1 i2))

(display addi) (newline) (center addi) (width addi) (show-percent addi) (newline)(newline)
(display subi) (newline) (center subi) (width subi) (show-percent subi) (newline)(newline)
(display muli) (newline) (center muli) (width muli) (show-percent muli) (newline)(newline)
(display divi) (newline) (center divi) (width divi) (show-percent subi) (newline)(newline)

(div-interval (make-interval 1 1) i3)
(mul-interval i3 (div-interval (make-interval 1 1) i3))
