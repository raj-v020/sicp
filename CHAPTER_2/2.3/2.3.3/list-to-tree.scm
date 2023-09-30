#lang scheme

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
    (cons '() elts)
    (let ((left-size (quotient (- n 1) 2)))
      (let ((left-result
              (partial-tree elts left-size)))
        (let ((left-tree (car left-result))
              (non-left-elts (cdr left-result))
              (right-size (- n (+ left-size 1))))
          (let ((this-entry (car non-left-elts))
                (right-result
                  (partial-tree
                    (cdr non-left-elts)
                    right-size)))
            (let ((right-tree (car right-result))
                  (remaining-elts
                    (cdr right-result)))
              (cons (make-tree this-entry
                               left-tree
                               right-tree)
                    remaining-elts))))))))
(provide list->tree)
#|
The partial-tree procedure takes as arguement the integer n and a list of n
elements, the base case if n = 0 the procedure returns the list provided as
arguement (i.e. null list); in other cases it creates a local variable which 
is the size of the list that will form the left tree and then recursively calls
itself by passing the arguements the given list and the local variable left-size
which return the left-tree and the remaining list that contains the entry node
and the right-list that will form the right-tree, the procedure then stores the
entry node in a local variable and recursively calls itself again by providing 
with the right-list/(cdr non-left-elts) and the size of the list that will form
the right tree and then store the result in variable right-tree and remaining-elts
and returns a pair whose car is the tree and whose cdr is the remaining elements.
|#


#|
IN SHORT:

1. Split the elements into left half, middle element, and right half
2. Build the left tree with the left half
3. Build the right tree with the right half
4. Return a built tree with the middle element, the left tree, and the right tree
|#

;(list->tree (list 1 3 5 7 9 11))

#|
                5
        1               9
            3       7       11
|#
