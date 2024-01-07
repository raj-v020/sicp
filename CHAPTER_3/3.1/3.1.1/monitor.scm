#lang scheme

(define (make-monitored f)
  (define count 0)
  (define (mf arg)
    (cond ((eq? arg 'how-many-calls?) count)
          ((eq? arg 'reset-count) (set! count 0))
          (else (begin (set! count (+ count 1)) (f arg)))))
    
  mf)
