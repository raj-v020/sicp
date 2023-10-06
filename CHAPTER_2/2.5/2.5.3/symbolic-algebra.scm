#lang scheme

(require "./general-arithmetic.scm")

(define (install-sparse-package)
  (define (tag x) (attach-tag 'sparse x))
  (define (coeff term) (cadr term))
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (tag (cdr term-list)))
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (empty-termlist? term-list) (null? term-list))
  
  (define (make-sparse variable term-list)
    (cons variable (tag term-list)))

  (put 'first-term '(sparse) first-term)
  (put 'rest-terms '(sparse) rest-terms)
  (put 'adjoin-term '(sparse) adjoin-term)
  (put 'empty? '(sparse) empty-termlist?)
  (put 'make 'sparse make-sparse)
  'done)

(define (install-dense-package)
  (define (tag x) (attach-tag 'dense x))
  (define (make-dense variable term-list)
    (cons variable (tag term-list)))
  (define (rest-terms term-list) (tag (cdr term-list)))
  (define (first-term term-list) (list (- (length term-list) 1) (car term-list)))
  (define (adjoin-term term term-list)
    (if (=zero? term)
        term-list
        (cons term term-list)))
  (define (empty-termlist? term-list) (null? term-list))
  
  (put 'first-term '(dense) first-term)
  (put 'rest-terms '(dense) rest-terms)
  (put 'adjoin-term '(dense) adjoin-term)
  (put 'empty? '(dense) empty-termlist?)
  (put 'make 'dense make-dense)
  'done)

(define (install-polynomial-package)
  (define (make-dense-poly variable term-list) ((get 'make 'dense) variable term-list))
  (define (make-sparse-poly variable term-list) ((get 'make 'sparse) variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (sparse? p) (if (list? (apply-generic 'first-term (term-list p))) #t #f))
  (define (dense? p) (not (sparse? p)))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  
  (define (the-empty-termlist) '())
  (define (make-term order coeff) (attach-tag 'sparse (list order coeff)))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (if (or (sparse? p1) (sparse? p2))
            (make-sparse-poly (variable p1)
                              (add-terms (term-list p1) (term-list p2)))
            (make-dense-poly (variable p1)
                             (add-terms (term-list p1) (term-list p2))))
        (error "Polys not in same var: ADD-POLY" (list p1 p2))))
  
  (define (add-terms L1 L2)
    (cond ((apply-generic 'empty? L1) (if (apply-generic 'empty? L2) (the-empty-termlist) L2))
          ((apply-generic 'empty? L2) (if (apply-generic 'empty? L1) (the-empty-termlist) L1))
          (else
           (let ((t1 (apply-generic 'first-term L1))
                 (t2 (apply-generic 'first-term L2)))
             (cond ((> (order t1) (order t2))
                    (apply-generic 'adjoin-term
                     t1 (add-terms (apply-generic 'rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (apply-generic 'adjoin-term
                     t2 (add-terms L1 (apply-generic 'rest-terms L2))))
                   (else
                    (apply-generic 'adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (apply-generic 'rest-terms L1)
                                (apply-generic 'rest-terms L2)))))))))
  
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (if (or (sparse? p1) (sparse? p2))
            (make-dense-poly (variable p1)
                             (mul-terms (term-list p1) (term-list p2)))
            (make-sparse-poly (variable p1)
                              (mul-terms (term-list p1) (term-list p2))))
        (error "Polys not in same var: MUL-POLY" (list p1 p2))))

  (define (mul-terms L1 L2)
    (if (apply-generic 'empty? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (apply-generic 'first-term L1) L2)
                   (mul-terms (apply-generic 'rest-terms L1) L2))))
  
  (define (mul-term-by-all-terms t1 L)
    (if (apply-generic 'empty? L)
        (the-empty-termlist)
        (let ((t2 (apply-generic 'first-term L)))
          (apply-generic 'adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (apply-generic 'rest-terms L))))))
  
  (define (tag p) (attach-tag 'polynomial p))
  (define (=zero?-poly p)
    (if (= (length (contents (term-list p))) 1)
        (if (= (coeff (apply-generic 'first-term (term-list p))) 0) #t
            #f)
        #f))

  (define (negate-poly term-list)
    (if (apply-generic 'empty? term-list)
        (the-empty-termlist)
        (apply-generic 'adjoin-term (make-term (order (apply-generic 'first-term term-list))
                                (- (coeff (apply-generic 'first-term term-list))))
                     (negate-poly (apply-generic 'rest-terms term-list)))))
  
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1
                                      (if (dense? p2)
                                          (make-dense-poly (variable p2)
                                                           (negate-poly (term-list p2)))
                                          (make-sparse-poly (variable p2)
                                                            (negate-poly (term-list p2))))))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put '=zero? '(polynomial) =zero?-poly)
  (put 'negation '(polynomial) (lambda (p) (negate-poly (term-list p))))
  (put 'variable '(polynomial) variable)
  (put 'term-list '(polynomial) term-list)
  (put 'make-dense 'polynomial
       (lambda (var terms) (tag (make-dense-poly var terms))))
  (put 'make-sparse 'polynomial
       (lambda (var terms) (tag (make-sparse-poly var terms))))
  'done)

(define (negation object)
  (apply-generic 'negation object))

(define (make-dense-polynomial var terms)
  ((get 'make-dense 'polynomial) var terms))
(define (make-sparse-polynomial var terms)
  ((get 'make-sparse 'polynomial) var terms))

(install-sparse-package)
(install-dense-package)
(install-polynomial-package)

(define x (make-rational 1 4))
(define z1 (make-complex-from-real-imag 3 -4))
(define z2 (make-complex-from-real-imag 3 4))
(define z3 (make-complex-from-mag-ang 5 pi))

(define term-list-1 (list (list 100 1) (list 1 1) (list 0 1)))
(define term-list-2 (list 3 1 2))
(define p1 (make-sparse-polynomial 'x term-list-1))
(define p2 (make-dense-polynomial 'x term-list-2))
(define p0s (make-sparse-polynomial 'x '((0 0))))
(define p0d (make-dense-polynomial 'x '(0)))
(add p1 p2)
z1
(=zero? p0s)
(=zero? p0d)