#lang sicp
;n = 5
;sum = 10
; i = 5 ;j = 4 ;k = 1
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))
(define (ordered-triples s n)
  (filter (lambda (x) (= s (+ (car x)
                              (cadr x)
                              (caddr x))))
          (flatmap (lambda (x) x)
                   (flatmap
                    (lambda (i)
                      (map (lambda (j)
                             (map (lambda (k)
                                    (list i j k))
                                  (enumerate-interval 1 (- j 1))))
                           (enumerate-interval 1 (- i 1))))
                    (enumerate-interval 1 n)))))


(ordered-triples 10 5)