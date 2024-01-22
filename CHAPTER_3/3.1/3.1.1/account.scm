#lang scheme

(define (insert l element)
  (if (null? l) (cons element '()) 
    (cons (car l) (insert (cdr l) element))))

(define (contains? l element)
  (cond ((null? l) #f)
        ((eq? (car l) element) #t)
        (else (contains? (cdr l) element))))

(define (make-account balance password)
  (define passwords (list password))
  (define incorrect-attempts 0)
  (define (monitor-attempts arg)
    (cond ((eq? arg 'how-many-attempts?) incorrect-attempts)
          ((eq? arg 'reset-attempts) (set! incorrect-attempts 0))
          ((eq? arg 'add-attempts) (set! incorrect-attempts (+ incorrect-attempts 1)))))

  (define (withdraw amount)
      (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))

  (define (deposit amount)
      (begin (set! balance (+ balance amount))
             balance))

  (define (dispatch p m)
    (cond ((not (contains? passwords p)) (begin (monitor-attempts 'add-attempts)
                                         (if (= (monitor-attempts 'how-many-attempts?) 8) 
                                           (lambda (x) "Calling Cops")
                                           (lambda (x) "Incorrect Password"))))
          ((eq? m 'withdraw) (begin (monitor-attempts 'reset-attempts) withdraw))
          ((eq? m 'deposit) (begin (monitor-attempts 'reset-attempts) deposit))
          ((eq? m 'check-balance) balance)
          ((eq? m 'make-joint) (begin (monitor-attempts 'reset-attempts)
                                      (lambda (pass) (set! passwords (insert passwords pass))
                                        dispatch)))
          (else (error "Unknown request: MAKE-ACCOUNT"
                       m))))
  dispatch)

(define (make-joint acc og-pass new-pass)
  ((acc og-pass 'make-joint) new-pass))
  
(define paul-acc (make-account 1000 'open-sesame))

(define peter-acc 
  (make-joint paul-acc 'open-sesame 'rosebud))
