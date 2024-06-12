#lang sicp

(define stream-car car)
(define (stream-cdr s) (force (cdr s)))

(define (square x) (* x x))
(define (cube x) (* x x x))
(define (divisible? x n) (= (remainder x n) 0)) 
(define ones (cons-stream 1 ones))

(define (display-line x) (display x) (newline))

(define (display-stream s n)
  (if (zero? n)
      (display-line "done")
      (begin
        (display-line (stream-car s))
        (display-stream (stream-cdr s) (- n 1) ))))

(define (stream-map f . ss)
  (if (stream-null? (car ss))
      the-empty-stream
      (cons-stream (apply f (map stream-car ss))
                   (apply stream-map f (map stream-cdr ss)))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter
                       pred
                       (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (add-streams s1 s2) (stream-map + s1 s2))
(define (mul-streams s1 s2) (stream-map * s1 s2))

(define integers
  (cons-stream 1 (add-streams ones integers)))

(define (stream-append s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (stream-append (stream-cdr s1) s2))))

(define (merge-weighted st1 st2 weight)
  (define (merge s1 s2)
    (cond ((stream-null? s1) s2)
          ((stream-null? s2) s1)
          (else
           (let ((s1car (stream-car s1))
                 (s2car (stream-car s2)))
             (if (<= (weight s1car) (weight s2car))
                 (cons-stream
                  s1car
                  (merge (stream-cdr s1) s2))
                 (cons-stream
                  s2car
                  (merge s1 (stream-cdr s2))))))))
  (merge st1 st2))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
    weight)))

#|(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (interleave (stream-map (lambda (x) (list (stream-car s) x))
                            (stream-cdr t))
                (stream-map (lambda (x) (list x (stream-car t)))
                            (stream-cdr s)))
    (pairs (stream-cdr s) (stream-cdr t)))))|#

(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave (stream-map (lambda (p) (append (list (stream-car s)) p))
                           (stream-cdr (pairs t u)))
               (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

#|(define pythagorean-triples (stream-filter (lambda (triple) (= (+ (square (car triple))
                                                                  (square (cadr triple)))
                                                               (square (caddr triple))))
                                           (triples integers integers integers)))

(display-stream pythagorean-triples 5)|#

#|(define filtered-integers (stream-filter (lambda (x) (and (not (divisible? x 2))
                                                          (not (divisible? x 3))
                                                          (not (divisible? x 5))))
                                         integers))
(define pairs1 (weighted-pairs integers integers (lambda (p) (+ (car p) (cadr p)))))
(define pairs2 (weighted-pairs filtered-integers filtered-integers (lambda (p) (+ (* 2 (car p))
                                                                                  (* 3 (cadr p))
                                                                                  (* 5 (car p) (cadr p))))))
(display-stream pairs1 10)
(display-stream pairs2 10)|#

#|(define (filter-consecutive-two weight stream)
  (define (iter prev s)
    (if (= (weight prev)
           (weight (stream-car s)))
        (cons-stream prev
                     (cons-stream (stream-car s)
                                  (iter (stream-car s) (stream-cdr s))))
        (iter (stream-car s) (stream-cdr s))))
  (iter (stream-car stream) (stream-cdr stream)))

(define w1 (lambda (p) (+ (cube (car p))
                              (cube (cadr p)))))

(define ramanujan-numbers (filter-consecutive-two w1 (weighted-pairs integers integers w1)))

(display-stream (stream-map (lambda (p) (list p (w1 p))) ramanujan-numbers) 12)|#

#|(define (filter-consecutive-three weight stream)
  (let ((s1 (stream-car stream))
        (s2 (stream-car (stream-cdr stream)))
        (s3 (stream-car (stream-cdr (stream-cdr stream)))))
    (let ((w1 (weight s1))
          (w2 (weight s2))
          (w3 (weight s3)))
      (if (= w1 w2 w3)
          (cons-stream s1
                       (cons-stream s2
                                       (cons-stream s3
                                                       (filter-consecutive-three weight
                                                                                 (stream-cdr stream)))))
          (filter-consecutive-three weight (stream-cdr stream))))))

(define w2 (lambda (p) (+ (square (car p))
                          (square (cadr p)))))
(define stream1 (filter-consecutive-three w2 (weighted-pairs integers integers w2)))

(display-stream (stream-map (lambda (p) (list p (w2 p))) stream1) 15)|#



  