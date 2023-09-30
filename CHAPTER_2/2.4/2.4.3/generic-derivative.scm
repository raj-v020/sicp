#lang racket

(define *the-table* (make-hash));make THE table
 (define (put key1 key2 value) (hash-set! *the-table* (list key1 key2) value));put
 (define (get key1 key2) (hash-ref *the-table* (list key1 key2) #f));get

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num) (and (number? exp) (= exp num)))

(define (install-sum-package)
  (define (addend s)
    (car s))
  (define (augend s)
    (if (> (length s) 2) (make-sum (cadr s) (caddr s))
      (cadr s)))
  (define (sum? x) (and (pair? x) (eq? (car x) '+)))

  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2))
           (+ a1 a2))
          (else (append '(+) (if (sum? a1) (append '() (cdr a1)) (list a1))
                        (if (sum? a2) (append '() (cdr a2)) (list a2))))))

  (define (sum-deriv exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))

  (put 'make-exp '+ make-sum)
  (put 'deriv '+ sum-deriv)
  'done)

(define (install-product-package)
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (append '(*) (if (product? m1) (append '() (cdr m1)) (list m1))
                        (if (product? m2) (append '() (cdr m2)) (list m2))))))

  (define (product? x) (and (pair? x) (eq? (car x) '*)))
  (define (multiplier p) (car p))
  (define (multiplicand p) (if (> (length p) 2) (make-product (cadr p) (caddr p))
                             (cadr p)))

  (define (product-deriv exp var)
    ((get 'make-exp '+)
     (make-product (multiplier exp)
                   (deriv (multiplicand exp) var))
     (make-product (deriv (multiplier exp) var)
                   (multiplicand exp))))

  (put 'make-exp '* make-product)
  (put 'deriv '* product-deriv)
  'done)

(define (install-exponentiation-package)
  (define (make-exponentiation b n)
    (cond ((or (=number? n 0) (=number? b 1)) 1)
          ((=number? b 0) 0)
          ((=number? n 1) b)
          ((and (number? b) (number? n)) (expt b n))
          (else (list '** b n))))
  (define (base s) (car s))
  (define (exponent s) (cadr s))

  (define (exponent-deriv exp var)
    ((get 'make-exp '*)
    ((get 'make-exp '*) (exponent exp)
                  (make-exponentiation (base exp)
                                       (- (exponent exp) 1)))
    (deriv (base exp) var)))

  (put 'make-exp '** make-exponentiation)
  (put 'deriv '** exponent-deriv)
  'done)

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
               (operands exp) var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

#|
(install-sum-package)
(install-product-package)
(install-exponentiation-package)

(deriv '(+ (* x y (+ x 3)) (** x 3)) 'x)
|#
