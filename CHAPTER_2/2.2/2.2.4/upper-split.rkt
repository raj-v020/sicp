#lang sicp
(define (upper-split painter n)
  (if (= n 0)
      (let ((smaller-upper-split (upper-split painter (- n 1))))
            (below painter (beside smaller-upper-split smaller-upper-split)))))
