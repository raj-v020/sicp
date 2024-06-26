#lang swindle

(define (require p) (if (not p) (amb)))
(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (multiple-dwelling)
  (let ((cooper (amb 2 3 4 5))
        (miller (amb 3 4 5)))
    (require (> miller cooper))
    (let ((fletcher (amb 2 3 4)))
      (require (not (= (abs (- fletcher cooper)) 1)))
      (let ((smith (amb 1 2 3 4 5)))
        (require (not (= (abs (- smith fletcher)) 1)))
        (let ((baker (amb 1 2 3 4)))
          (require
            (distinct? (list baker cooper fletcher miller smith)))
          (list (list 'baker baker)
                (list 'cooper cooper)
                (list 'fletcher fletcher)
                (list 'miller miller)
                (list 'smith smith)))))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (baker x)
  (car x))
(define (cooper x)
  (cadr x))
(define (fletcher x)
  (caddr x))
(define (miller x)
  (cadddr x))
(define (smith x)
  (car (cddddr x)))

(define (multiple-dwelling-1)
  ; baker cooper fletcher miller smith
  (define dwelling (car (filter (lambda (x)
                             (and (not (= (baker x) 5))
                                  (not (= (cooper x) 1))
                                  (not (= (fletcher x) 5))
                                  (not (= (fletcher x) 1))
                                  (> (miller x) (cooper x))
                                  (not (= (abs (- (fletcher x) (cooper x))) 1))
                                  (not (= (abs (- (smith x) (fletcher x))) 1))))
                           (permutations (enumerate-interval 1 5)))))
  (list (list 'baker (baker dwelling))
        (list 'cooper (cooper dwelling))
        (list 'fletcher (fletcher dwelling))
        (list 'miller (miller dwelling))
        (list 'smith (smith dwelling))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (remove s seq)
  (filter (lambda (x) (not (= x s)))
          seq))

(define (permutations seq)
  (cond ((null? seq) '())
        ((= (length seq) 1) (list seq))
        (else (accumulate append '() (map (lambda (s)
                                            (map (lambda (perm)
                                                   (append (list s) perm))
                                                 (permutations (remove s seq)))) seq)))))
(println (multiple-dwelling))
(display (multiple-dwelling-1))

(define (xor a b)
  (and (or a b) (not (and a b))))

(define (liars)
  (let ((betty (amb 1 2 3 4 5))
        (ethel (amb 1 2 3 4 5))
        (joan (amb 1 2 3 4 5))
        (kitty (amb 1 2 3 4 5))
        (mary (amb 1 2 3 4 5)))
    (require (xor (= kitty 2) (= betty 3)))
    (require (xor (= ethel 1) (= joan 2)))
    (require (xor (= joan 3) (= ethel 5)))
    (require (xor (= kitty 2) (= mary 4)))
    (require (xor (= mary 4) (= betty 1)))
    (require
      (distinct? (list betty ethel joan kitty mary)))
    (list (list 'kitty kitty)
          (list 'joan joan)
          (list 'betty betty)
          (list 'mary mary)
          (list 'ethel ethel))))

(define (daughters)
  ;1 - gabrielle
  ;2 - mary
  ;3 - lorna
  ;4 - rosalind
  ;5 - melissa
  (let ((moore (cons (amb 1 2 3 4 5) 3))
        (downing (cons (amb 1 2 3 4 5) 5))
        (hall (cons (amb 1 2 3 4 5) 4))
        (hood (cons 5 1))
        (parker (cons (amb 1 2 3 4 5) 2)))
    (require (not (= (car moore) (cdr moore))))
    (require (not (= (car downing) (cdr downing))))
    (require (not (= (car hall) (cdr hall))))
    (require (not (= (car parker) (cdr parker))))
    (require (not (= (car parker) 1)))
    (require (cond ((= 1 (car downing)) (= (car parker) (cdr downing)))
                   ((= 1 (car hall)) (= (car parker) (cdr hall)))
                   ((= 1 (car moore)) (= (car parker) (cdr moore)))))
    (require
      (distinct? (map car (list moore downing hall hood parker))))
    (list (list 'moore moore)
          (list 'downing downing)
          (list 'hall hall)
          (list 'hood hood)
          (list 'parker parker))))

(define (eight-queens)
  (let ((row1 (amb 1 2 3 4 5 6 7 8))
        (row2 (amb 1 2 3 4 5 6 7 8))
        (row3 (amb 1 2 3 4 5 6 7 8))
        (row4 (amb 1 2 3 4 5 6 7 8))
        (row5 (amb 1 2 3 4 5 6 7 8))
        (row6 (amb 1 2 3 4 5 6 7 8))
        (row7 (amb 1 2 3 4 5 6 7 8))
        (row8 (amb 1 2 3 4 5 6 7 8)))
    (require (not (= row1 row2)))
    (require (not (= row1 row3)))
    (require (not (= row1 row4)))
    (require (not (= row1 row5)))
    (require (not (= row1 row6)))
    (require (not (= row1 row7)))
    (require (not (= row1 row8)))
    
    (require (not (= row2 row3)))
    (require (not (= row2 row4)))
    (require (not (= row2 row5)))
    (require (not (= row2 row6)))
    (require (not (= row2 row7)))
    (require (not (= row2 row8)))

    (require (not (= row3 row4)))
    (require (not (= row3 row5)))
    (require (not (= row3 row6)))
    (require (not (= row3 row7)))
    (require (not (= row3 row8)))

    (require (not (= row4 row5)))
    (require (not (= row4 row6)))
    (require (not (= row4 row7)))
    (require (not (= row4 row8)))

    (require (not (= row5 row6)))
    (require (not (= row5 row7)))
    (require (not (= row5 row8)))

    (require (not (= row6 row7)))
    (require (not (= row6 row8)))

    (require (not (= row7 row8)))

    (require (not (= 1 (abs (- row1 row2)))))
    (require (not (= 2 (abs (- row1 row3)))))
    (require (not (= 3 (abs (- row1 row4)))))
    (require (not (= 4 (abs (- row1 row5)))))
    (require (not (= 5 (abs (- row1 row6)))))
    (require (not (= 6 (abs (- row1 row7)))))
    (require (not (= 7 (abs (- row1 row8)))))
    
    (require (not (= 1 (abs (- row2 row3)))))
    (require (not (= 2 (abs (- row2 row4)))))
    (require (not (= 3 (abs (- row2 row5)))))
    (require (not (= 4 (abs (- row2 row6)))))
    (require (not (= 5 (abs (- row2 row7)))))
    (require (not (= 6 (abs (- row2 row8)))))

    (require (not (= 1 (abs (- row3 row4)))))
    (require (not (= 2 (abs (- row3 row5)))))
    (require (not (= 3 (abs (- row3 row6)))))
    (require (not (= 4 (abs (- row3 row7)))))
    (require (not (= 5 (abs (- row3 row8)))))

    (require (not (= 1 (abs (- row4 row5)))))
    (require (not (= 2 (abs (- row4 row6)))))
    (require (not (= 3 (abs (- row4 row7)))))
    (require (not (= 4 (abs (- row4 row8)))))

    (require (not (= 1 (abs (- row5 row6)))))
    (require (not (= 2 (abs (- row5 row7)))))
    (require (not (= 3 (abs (- row5 row8)))))

    (require (not (= 1 (abs (- row6 row7)))))
    (require (not (= 2 (abs (- row6 row8)))))

    (require (not (= 1 (abs (- row7 row8)))))
    
    (list (list 1 row1)
          (list 2 row2)
          (list 3 row3)
          (list 4 row4)
          (list 5 row5)
          (list 6 row6)
          (list 7 row7)
          (list 8 row8))))

(define (list-amb lst)
    (if (null? lst)
      (amb)
      (amb (car lst) (list-amb (cdr lst)))))

(define (nth i lst)
    (cond ((null? lst) '())
          ((= i 0) (car lst))
          (else (nth (- i 1) (cdr lst)))))

(define (attacks? row1 col1 row2 col2)
    (cond ((= row1 row2) true)
          ((= col1 col2) true)
          ((= (abs (- col1 col2))
              (abs (- row1 row2))) true)
          (else false)))

(define (safe-kth? k pos)
    (let ((kth-col (nth k pos))
          (pos-len (length pos)))
      (define (safe-iter i)
        (cond ((= i pos-len) true)
              ((= i k) (safe-iter (+ i 1)))
              (else
                (let ((ith-col (nth i pos)))
                  (if (attacks? i ith-col k kth-col)
                    false
                    (safe-iter (+ i 1)))))))
      (safe-iter 0)))

(define (queens n)
    (define (queens-iter pos i)
      (cond ((> i (- n 1)) pos)
            (else
              (let ((new-col (list-amb (enumerate-interval 1 n))))
                (let ((new-pos (append pos (list new-col))))
                  (require (safe-kth? i new-pos))
                  (queens-iter new-pos (+ i 1)))))))
    (queens-iter '() 0))
