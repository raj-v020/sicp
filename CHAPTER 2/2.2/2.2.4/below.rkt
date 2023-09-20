#lang scheme
(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 .0)))
(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))
(define (below1 painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-top
           (transform-painter
            painter1
            split-point
            (make-vect 0.0 1.0)
            (make-vect 1.0 0.5)))
          (paint-bottom
           (transform-painter
            painter2
            (make-vect 0.0 0.0)
            (make-vect 1.0 0.0)
            split-point)))
      (λ (frame)
        (paint-bottom frame)
        (paint-top frame)))))

(define (below2 painter1 painter2)
  (rotate90 (beside (rotate270 painter1) (rotate270 pianter2))))