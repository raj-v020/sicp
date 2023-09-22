#lang sicp
(define (make-branch length structure)
  (cons length structure))

(define (branch-len branch)
  (car branch))
(define (branch-struc branch)
  (cdr branch))
(define (branch-torque branch)
  (* (car branch) (if (pair? (cdr branch))
                      (total-weight (cdr branch))
                      (cdr branch))))

(define (make-mobile left right)
  (cons left right))

(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cdr mobile))

;bsrb - branch structure of right branch
(define (total-weight mobile)
  (if (null? mobile) 0
      (let ((bslb (if (not (null? (left-branch mobile))) (branch-struc (left-branch mobile)) 0))
            (bsrb (if (not (null? (right-branch mobile))) (branch-struc (right-branch mobile)) 0)))
        (+ (if (pair? bslb) (total-weight bslb) bslb)
           (if (pair? bsrb) (total-weight bsrb) bsrb)))))
 
(define (is-balanced? mobile)
  (if (pair? mobile)
      (and (= (branch-torque (left-branch mobile)) (branch-torque (right-branch mobile)))
           (is-balanced? (branch-struc (left-branch mobile)))
           (is-balanced? (branch-struc (right-branch mobile)))) true))

(define l (make-branch 6 7))
(define r (make-branch 6 (make-mobile (make-branch 6 4) (make-branch 8 3))))
(define m (make-mobile l r))
(is-balanced? m)