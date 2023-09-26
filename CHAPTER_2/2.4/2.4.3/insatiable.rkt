#lang racket

(define *the-table* (make-hash));make THE table
(define (put key1 key2 value) (hash-set! *the-table* (list key1 key2) value));put
(define (get key1 key2) (hash-ref *the-table* (list key1 key2) #f));get

(define (attach-tag type-tag content) (cons type-tag content))
(define (get-record employee-name file)
  (attach-tag (division file)
              ((get 'get-record (division file)) employee-name file)))

(define (get-salary record)
  (let ((record-type (car record))
        (record-content (cdr record)))
    ((get 'get-salary record-type) record-content)))

(define (find-employee-record employee-name file-list)
  (if (null? file-list)
    #f
    (let ((current-file (car file-list)))
      (if (get-record employee-name current-file)
        (get-record employee-name current-file)
        (find-employee-record employee-name (cdr file-list))))))
