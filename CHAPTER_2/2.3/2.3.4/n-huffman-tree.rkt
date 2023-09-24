#lang scheme
(require "huffman-encoding.rkt")
(require "huffman-tree.rkt")

(define (n-alphabets n)
  (if (= n 0) '() (cons (list n (expt 2 (- n 1))) (n-alphabets (- n 1)))))

(define (n-huffman-tree n)
  (generate-huffman-tree (n-alphabets n)))

(define (encode-most-frequent-symbol s)
  (encode (list s) (n-huffman-tree s)))

(define (encode-least-frequent-symbol n)
  (encode (list 1) (n-huffman-tree n)))

(define k 20)

(define encoded-m-frequent-symbol
  (encode-most-frequent-symbol k))
(define encoded-l-frequent-symbol
  (encode-least-frequent-symbol k))

(define bits-most-frequent-symbol (length encoded-m-frequent-symbol))
(define bits-least-frequent-symbol (length encoded-l-frequent-symbol))

;encoded-m-frequent-symbol ; (1)
;bits-most-frequent-symbol ; 1 bit
;encoded-l-frequent-symbol ; sequence of (n - 1) 0's
;bits-least-frequent-symbol ; (n - 1) bits
