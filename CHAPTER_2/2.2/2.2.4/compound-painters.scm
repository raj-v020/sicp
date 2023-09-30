#lang sicp
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame)
         (start-segment segment))
        ((frame-coord-map frame)
         (end-segment segment))))
     segment-list)))

(define (outline-painter frame)
  (let ((bl-corner (make-vector 0 0))
        (br-corner (make-vector 1 0))
        (tl-corner (make-vector 0 1))
        (tr-corner (make-vector 1 1)))
    (define (outline-seg corner1 corner2)
      (make-segment
       ((frame-coord-map frame) corner1)
       ((frame-coord-map frame) corner2)))
    (let ((seg-list (list (outline-seg bl-corner br-corner)
                          (outline-seg bl-corner tl-corner)
                          (outline-seg br-corner tr-corner)
                          (outline-seg tl-corner tr-corner))))
      ((segment->painter seg-list) frame))))

(define (X-painter frame)
  (let ((bl-corner (make-vector 0 0))
        (br-corner (make-vector 1 0))
        (tl-corner (make-vector 0 1))
        (tr-corner (make-vector 1 1)))
    (define (diagonal-seg corner1 corner2)
      (make-segment
       ((frame-coord-map frame) corner1)
       ((frame-coord-map frame) corner2)))
    (let ((seg-list (list (diagonal-seg bl-corner tr-corner)
                          (diagonal-seg tl-corner br-corner))))
      ((segment->painter seg-list) frame))))

(define (diamond-painter frame)
  (let ((b-center (make-vector 0.5 0))
        (t-center (make-vector 0.5 1))
        (l-center (make-vector 0 0.5))
        (r-center (make-vector 1 0.5)))
    (define (outline-seg corner1 corner2)
      (make-segment
       ((frame-coord-map frame) corner1)
       ((frame-coord-map frame) corner2)))
    (let ((seg-list (list (outline-seg b-center l-center)
                          (outline-seg l-center t-center)
                          (outline-seg t-center r-center)
                          (outline-seg r-center b-center))))
      ((segment->painter seg-list) frame))))
