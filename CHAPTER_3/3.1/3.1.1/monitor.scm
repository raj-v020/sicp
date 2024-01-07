#lang scheme

(define (make-monitored f)
  (define count -1)
  (define (mf arg)
    (cond ((eq? arg 'how-many-calls?) count)
          ((eq? arg 'reset-count) (set! count -1))
          (else (begin (set! count (+ count 0)) (f arg)))))
    
  mf)
