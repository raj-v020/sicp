#lang sicp
(define (make-branch length structure)
  (list length structure))

(define (branch-len branch)
  (car branch))
(define (branch-struc branch)
  (cadr branch))
(define (branch-torque branch)
  (* (car branch) (if (list? (cadr branch))
                      (total-weight (cadr branch))
                      (cadr branch))))

(define (make-mobile left right)
  (list left right))

(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cadr mobile))

;bsrb - branch structure of right branch
(define (total-weight mobile)
  (let ((bslb (branch-struc (left-branch mobile)))
        (bsrb (branch-struc (right-branch mobile))))
    (+ (if (list? bslb) (total-weight bslb) bslb)
       (if (list? bsrb) (total-weight bsrb) bsrb))))

(define (is-balanced? mobile)
  (if (pair? mobile)
      (and (= (branch-torque (left-branch mobile)) (branch-torque (right-branch mobile)))
           (is-balanced? (branch-struc (left-branch mobile)))
           (is-balanced? (branch-struc (right-branch mobile)))) true))

(define l (make-branch 6 7))
(define r (make-branch 6 (make-mobile (make-branch 6 4) (make-branch 8 3))))
(define m (make-mobile l r))
(is-balanced? m)