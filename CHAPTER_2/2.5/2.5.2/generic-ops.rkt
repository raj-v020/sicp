#lang scheme

(define *the-table* (make-hash))
(define (put key1 key2 value) (hash-set! *the-table* (list key1 key2) value))
(define (get key1 key2) (hash-ref *the-table* (list key1 key2) #f))

(define (square x) (if (number? x) (* (contents x) (contents x)) (square (/ (numer x) (denom x)))))
(define (sine x) (sin (if (number? x) (contents x) (sine (/ (numer x) (denom x))))))
(define (cosine x) (cos (if (number? x) (contents x) (sine (/ (numer x) (denom x))))))
(define (arctan x y) (atan (if (number? x) x (/ (numer x) (denom x)))
                           (if (number? y) y (/ (numer y) (denom y)))))

(define (map-list l1 l2)
  (if (null? l1) '() (cons (list (car l1) (car l2)) (map-list (cdr l1) (cdr l2)))))

(define (apply-list l)
  (map (lambda (sub-l)
         (apply (car sub-l) (cdr sub-l))) l))

(define (list-ref x items)
  (if (equal? (car items) x)
      0
      (+ 1 (list-ref x (cdr items)))))
(define (compose f g) (lambda (x) (f (g x))))

(define (repeated f n)
  (define (repeatedi g i)
    (if (= i n) g (repeatedi (lambda (x) ((compose f g) x)) (+ i 1))))
  (repeatedi f 1))

(define (member? item x)
  (cond ((null? x) false)
        ((eq? item (car x)) #t)
        (else (member? item (cdr x)))))

(define tower '(integer rational real complex))
(define operations '(add sub mul div))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'integer)
        (else
         (error "Bad tagged datum: TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else
         (error "Bad tagged datum: CONTENTS" datum))))

(define (install-rectangular-package)
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (arctan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cosine a)) (* r (sine a))))
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  (define (magnitude z) (contents (car z)))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z) (* (magnitude z) (cosine (angle z))))
  (define (imag-part z) (* (magnitude z) (sine (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (arctan y x)))
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
(define (equ? num1 num2) (apply-generic 'equal? num1 num2))
(define (=zero? num) (apply-generic '=zero? num))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
  (define (raise-integer n)
    (make-rational n 1))
  (define (project-integer n)
    n)
  (put 'add '(integer integer) +)
  (put 'sub '(integer integer) -)
  (put 'mul '(integer integer) *)
  (put 'div '(integer integer) /)
  (put 'raise '(integer) raise-integer)
  (put 'project '(integer) project-integer)
  (put 'equal? '(integer integer) =)
  (put '=zero? '(integer) zero?)
  'done)

(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (tag x) (attach-tag 'rational x))
  (define (equal-rational? x y)
    (= (* (numer x) (denom y))
       (* (numer y) (denom x))))
  (define (=zero? x)
    (and (= (numer x) 0)
         (not (= (denom x) 0))))
  (define (raise-rational x)
    (make-real (/ (numer x) (denom x))))
  (define (project-rational x)
    (numer x))

  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'numer '(rational) numer)
  (put 'denom '(rational) denom)
  (put 'raise '(rational) raise-rational)
  (put 'project '(rational) project-rational)
  (put 'equal? '(rational rational) equal-rational?)
  (put '=zero? '(rational) =zero?)
  'done)

(define (numer x)
  (apply-generic 'numer x))
(define (denom x)
  (apply-generic 'denom x))
(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-real-package)
  (define (tag x) (attach-tag 'real x))
  (define (raise-real x)
    (make-complex-from-real-imag (contents x) 0))
  (define (project-real x)
    (round x))
  (put 'add '(real real) +)
  (put 'sub '(real real) -)
  (put 'mul '(real real) *)
  (put 'div '(real real) /)
  (put 'make 'real (lambda (x) (tag (* 1.0 x))))
  (put 'equal? '(real real) (lambda (x y) (= x y)))
  (put '=zero? '(real) (lambda (x) (= x 0)))
  (put 'raise '(real) raise-real)
  (put 'project '(real) project-real)
  'done)

(define (make-real x)
  ((get 'make 'real) (if (number? x) x (/ (numer x) (denom x)))))

(define (install-complex-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  (define (add-complex z1 z2)
    (make-from-real-imag (add (real-part z1) (real-part z2))
                         (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (real-part z1) (real-part z2))
                         (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                       (add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                       (sub (angle z1) (angle z2))))
  (define (equal-complex? z1 z2)
    (and (equ? (real-part z1) (real-part z2))
         (equ? (imag-part z1) (imag-part z2))))
  (define (=zero? z)
    (= (magnitude z) 0))
  (define (raise-complex z)
    (tag z))
  (define (project-complex z)
    (make-real (real-part z)))
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'raise '(complex) raise-complex)
  (put 'project '(complex) project-complex)
  (put 'equal? '(complex complex) equal-complex?)
  (put '=zero? '(complex) =zero?)
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(install-rectangular-package)
(install-polar-package)
(install-complex-package)
(install-rational-package)
(install-real-package)
(install-scheme-number-package)

(define *table-2* (make-hash))
(define (put-coercion key1 key2 value) (hash-set! *table-2* (list key1 key2) value))
(define (get-coercion key1 key2) (hash-ref *table-2* (list key1 key2) #f))

(define (apply-generic op . args)
  (define (show-error type-tags)
    (error "No method for these types" (list op type-tags)))

  (define (successive-raise lower-type higher-type)
    (let ((height-diff (- (list-ref higher-type tower) (list-ref lower-type tower))))
      (cond ((higher? lower-type higher-type) #f)
            ((eq? lower-type higher-type) (lambda (x) x))
            (else (repeated raise height-diff)))))

  (define (coerce-type type type-tags)
    (define (iter tags coerced-types)
      (cond ((null? tags) coerced-types)
            ((successive-raise (car tags) type) (iter (cdr tags)
                                                      (append coerced-types
                                                              (list (successive-raise (car tags) type)))))
            (else #f)))
    (iter type-tags '()))

  (define (coerce-args type-tags)
    (define (iter current-type)
      (cond ((null? current-type) #f)
            ((coerce-type (car current-type) type-tags) (apply-list (map-list (coerce-type (car current-type)
                                                                                           type-tags)
                                                                              args)))
            (else (iter (cdr current-type)))))
    (iter type-tags))

  (define (coerced? type-tags)
    (define (iter first-type tags)
      (cond ((null? tags) #t)
            ((eq? first-type (car tags)) (iter first-type (cdr tags)))
            (else #f)))
    (iter (car type-tags) (cdr type-tags)))

  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (if (member? op operations)
              (drop (apply proc (map contents args)))
              (apply proc (map contents args)))
          (if (not (coerced? type-tags))
              (let ((coerced-args (coerce-args type-tags)))
                (if coerced-args
                    (apply apply-generic (append (list op) coerced-args))
                    (show-error type-tags)))
              (show-error type-tags))))))

(define (higher? x y)
  (if (> (list-ref x tower) (list-ref y tower)) #t #f))

(define (raise num)
  (apply-generic 'raise num))

(define (project num)
  (apply-generic 'project num))

(define (drop num)
  (if (eq? (type-tag num) 'integer) num
      (let ((raised-project-num (raise (project num))))
        (if (equ? num raised-project-num)
            (drop (project num))
            num))))