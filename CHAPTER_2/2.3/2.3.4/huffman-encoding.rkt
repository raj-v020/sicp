#lang scheme
(require "exercise-2.67.rkt")
(require "huffman-representation.rkt")

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) true)
        (else (memq item (cdr x)))))

(define (encode message tree)
  (if (null? message)
    '()
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (cond ((leaf? tree) '())
        ((memq symbol (symbols (left-branch tree)))
               (cons 0 (encode-symbol symbol (left-branch tree))))
        ((memq symbol (symbols (right-branch tree)))
         (cons 1 (encode-symbol symbol (right-branch tree))))
        (else (error "symbol not in tree: ENCODE-SYMBOL" symbol))))

(encode '(A D A B B C A) sample-tree)

(provide (all-defined-out))
