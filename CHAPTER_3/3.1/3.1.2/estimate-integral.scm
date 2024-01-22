#lang scheme

(define (sqr a) (* a a))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
            (iter (- trials-remaining 1)
                  trials-passed))))
  (iter trials 0))

(define (estimate-integral predicate trials x1 y1 x2 y2)
  (let ((area-rect (abs (* (- x2 x1) (- y2 y1)))))
    (* area-rect (monte-carlo trials 
                              (lambda () (predicate (random-in-range x1 x2)
                                                           (random-in-range y1 y2)))))))

(define (unit-circle? x y)
  (or (< (+ (sqr x) (sqr y)) 1) 
      (= (+ (sqr x) (sqr y)) 1)))

(exact->inexact (estimate-integral unit-circle? 1000000 -1 -1 1 1))
