#lang scheme
(require "huffman-representation.rkt")
(require "weighted-sets.rkt")

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge initial-pairs)
  (if (null? (cdr initial-pairs)) (car initial-pairs) 
    (let ((pair1 (car initial-pairs))
          (pair2 (cadr initial-pairs)))
      (successive-merge (adjoin-set 
                          (make-code-tree pair1 pair2) 
                          (cddr initial-pairs))))))

;(generate-huffman-tree '((A 8) (B 3) (C 1) (D 1) (E 1) (F 1) (G 1) (H 1)))

(provide (all-defined-out))

