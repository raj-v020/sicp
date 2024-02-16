#lang sicp

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (set-front-ptr! item) (set! front-ptr item))
    (define (set-rear-ptr! item) (set! rear-ptr item))
    (define (empty?) (null? front-ptr))
    (define (front-queue) (if (empty?) (error "FRONT called with an empty queue" (print))
                              (car front-ptr)))
    (define (insert new-pair) (cond ((empty?) (set-front-ptr! new-pair)
                                              (set-rear-ptr! new-pair)
                                              (print))
                                    (else (set-cdr! rear-ptr new-pair)
                                          (set-rear-ptr! new-pair)
                                          (print))))
    (define (delete) (cond ((empty?) (error "DELETE! called with an empty queue" (print)))
                           (else (set-front-ptr! (cdr front-ptr))
                                 (print))))
    (define (print) front-ptr)
    (define (dispatch m)
      (cond ((eq? m 'empty?) empty?)
            ((eq? m 'insert) insert)
            ((eq? m 'delete) delete)
            ((eq? m 'print) print)
            (else (error "QUEUE wrong dispatch message, MESSAGE: " m))))
    dispatch))

(define (empty-queue? queue)
  (queue 'empty?))
(define (insert-queue! queue item)
  ((queue 'insert) (cons item '())))
(define (delete-queue! queue)
  ((queue 'delete)))
(define (print-queue queue)
  ((queue 'print)))

(define q1 (make-queue))
(insert-queue! q1 'a)
(insert-queue! q1 'b)
(delete-queue! q1)
(delete-queue! q1)
