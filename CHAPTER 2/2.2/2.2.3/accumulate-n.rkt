#lang sicp
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (carseqs seqs)
  (if (null? seqs)
      nil
      (cons (caar seqs) (carseqs (cdr seqs)))))

(define (cdrseqs seqs)
  (if (null? seqs)
      nil
      (cons (cdar seqs) (cdrseqs (cdr seqs)))))
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))
(define sos (list (list 1 2 3)
                  (list 4 5 6)
                  (list 7 8 9)
                  (list 10 11 12)))

(carseqs sos)
(cdrseqs sos)
(accumulate-n + 0 sos)