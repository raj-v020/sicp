#lang sicp

(define (opposite-pairs? a b)
  (if (> a 0) (< b 0) (> b 0)))
(define (positive-pairs? a b)
  (if (opposite-pairs? a b) #f (> a 0)))
(define (negative-pairs? a b)
  (if (opposite-pairs? a b) #f (< a 0)))

(define (make-interval c p)
  (let ((w (/ (* c p) 100)))
    (cons (* 1.0 (min (- c w) (+ c w)))
          (* 1.0 (max (- c w) (+ c w))))))

(define (show-cw-interval i)
  (cons (center i) (width i)))

(define (show-cp-interval i)
  (cons (center i) (percent i)))

(define (lower-bound i)
  (car i))
(define (upper-bound i)
  (cdr i))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2.0))

(define (center i)
  (+ (lower-bound i) (width i)))

(define (percent i)
  (abs (/ (* 100 (width i)) (center i))))

(define (make-width lb ub)
  (/ (- ub lb) 2.0))

(define (make-center lb ub)
  (+ lb (make-width lb ub)))

(define (make-percent lb ub)
  (abs (/ (* 100 (make-width lb ub)) (make-center lb ub))))

(define (add-interval x y)
  (make-interval (make-center (+ (lower-bound x) (lower-bound y))
                              (+ (upper-bound x) (upper-bound y)))
                 (make-percent (+ (lower-bound x) (lower-bound y))
                               (+ (upper-bound x) (upper-bound y)))))

(define (o-mul-interval x y)
  (let ((lx (lower-bound x))
        (ux (upper-bound x))
        (ly (lower-bound y))
        (uy (upper-bound y)))
    (let ((p1 (* lx ly))
          (p2 (* lx uy))
          (p3 (* ux ly))
          (p4 (* ux uy)))
      (make-interval (make-center (min p1 p2 p3 p4)
                                  (max p1 p2 p3 p4))
                     (make-percent (min p1 p2 p3 p4)
                                   (max p1 p2 p3 p4))))))

(define (mul-interval x y)
  (let ((lx (lower-bound x))
        (ux (upper-bound x))
        (ly (lower-bound y))
        (uy (upper-bound y)))
    (cond ((positive-pairs? lx ux) (cond ((positive-pairs? ly uy)
                                          (make-interval (make-center (* lx ly) (* ux uy))
                                                         (make-percent (* lx ly) (* ux uy))))
                                         ((negative-pairs? ly uy)
                                          (make-interval (make-center (* ux ly) (* lx uy))
                                                         (make-percent (* ux ly) (* lx uy))))
                                         (else
                                          (make-interval (make-center (* ux ly) (* ux uy))
                                                         (make-percent (* ux ly) (* ux uy))))))
          
          ((negative-pairs? lx ux) (cond ((positive-pairs? ly uy)
                                          (make-interval (make-center (* lx uy) (* ux ly))
                                                         (make-percent (* lx uy) (* ux ly))))
                                         ((negative-pairs? ly uy)
                                          (make-interval (make-center (* ux uy) (* lx ly))
                                                         (make-percent (* ux uy) (* lx ly))))
                                         (else
                                          (make-interval (make-center (* lx uy) (* lx ly))
                                                         (make-percent (* lx uy) (* lx ly))))))
          
          (else (cond ((positive-pairs? ly uy)
                       (make-interval (make-center (* lx uy) (* ux uy))
                                      (make-percent (* lx uy) (* ux uy))))
                      ((negative-pairs? ly uy)
                       (make-interval (make-center (* ux ly) (* lx ly))
                                      (make-percent (* ux ly) (* lx ly))))
                      (else
                       (make-interval (make-center (min (* lx uy) (* ux ly))
                                                   (max (* lx ly) (* ux uy)))
                                      (make-percent (min (* lx uy) (* ux ly))
                                                    (max (* lx ly) (* ux uy))))))))))

(define (div-interval x y)
  (if (= (upper-bound y) (lower-bound y)) (error "DIVISION by ZERO interval")
      (mul-interval
       x
       (make-interval (make-center (/ 1.0 (upper-bound y)) (/ 1.0 (lower-bound y)))
                      (make-percent (/ 1.0 (upper-bound y)) (/ 1.0 (lower-bound y)))))))

(define (sub-interval x y)
  (make-interval (make-center (- (lower-bound y) (lower-bound x))
                              (- (upper-bound y) (upper-bound x)))
                 (make-percent (- (lower-bound y) (lower-bound x))
                               (- (upper-bound y) (upper-bound x)))))

(define i1 (make-interval 10 0.15))
(define i2 (make-interval 40 0.5))

(define addi (add-interval i1 i2))
(define subi (sub-interval i1 i2))
(define muli (mul-interval i1 i2))
(define divi (div-interval i1 i2))

(define (show-data f)
  (display " center-percent form: ") (display (show-cp-interval f)) (newline)
  (display " center-width form: ") (display (show-cw-interval f)) (newline)
  (display " bound form: ") (display f) (newline))

;(show-data addi) (newline)
;(show-data subi) (newline)
;(show-data muli) (newline)
;(show-data divi)
;(display "______________________________________") (newline) (newline)
(show-data i1) (newline)
(show-data i2) (newline)



(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 0)))
    (div-interval
     one (add-interval (div-interval one r1)
                       (div-interval one r2)))))

(define one (make-interval 1 0))
;(show-data (add-interval i1 i2)) (newline)
;(show-data (add-interval (div-interval one i1)
                       ;(div-interval one i2))) (newline)
(show-data (div-interval one i1)) (newline)
;(show-data (par1 i1 i2)) (newline)
;(show-data (par2 i1 i2))