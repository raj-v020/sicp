#lang scheme
(require "huffman-tree.rkt")
(require "huffman-encoding.rkt")


(define lyrics-alphabets '((A 2) (GET 2) (SHA 3) (WAH 1)
                                 (BOOM 1) (JOB 2) (NA 16) (YIP 9)))

(define lyrics-tree (generate-huffman-tree lyrics-alphabets))

(define song
  '(GET A JOB 
        SHA NA NA NA NA NA NA NA NA  
        GET A JOB 
        SHA NA NA NA NA NA NA NA NA  
        WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
        SHA BOOM))

(define encoded-song (encode song lyrics-tree))
;(length encoded-song)
;(* (log (length lyrics-alphabets) 2) (length song))
