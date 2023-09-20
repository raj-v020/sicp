#lang scheme
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num) (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (append '(+) (if (sum? a1) (append '() (cdr a1)) (list a1))
                      (if (sum? a2) (append '() (cdr a2)) (list a2))))))

(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (if (> (length s) 3) (make-sum (caddr s) (cadddr s))
                       (caddr s)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (append '(*) (if (product? m1) (append '() (cdr m1)) (list m1))
                      (if (product? m2) (append '() (cdr m2)) (list m2))))))

(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (if (> (length p) 3) (make-product (caddr p) (cadddr p))
                             (caddr p)))

(define (make-exponentiation b n)
  (cond ((or (=number? n 0) (=number? b 1)) 1)
        ((=number? b 0) 0)
        ((=number? n 1) b)
        ((and (number? b) (number? n)) (expt b n))
        (else (list '** b n))))
(define (exponentiation? x)
  (eq? (car x) '**))
(define (base s) (cadr s))
(define (exponent s) (caddr s))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((exponentiation? exp) (make-product
                                (make-product (exponent exp)
                                              (make-exponentiation (base exp)
                                                                   (- (exponent exp) 1)))
                                (deriv (base exp) var)))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error "unknown expression type: DERIV" exp))))

(deriv '(+ x (* x y) (** x 3)) 'x)