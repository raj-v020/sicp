#lang sicp

(define (make-table same-key?)
  (define (assoc key records)
    (cond ((null? records) false)
          ((same-key? key (caar records)) (car records))
          (else (assoc key (cdr records)))))

  (let ((local-table (list '*table*)))
    (define (lookup keys)
      (define (iter subtable k-list)
        (if (null? k-list)
            (cdr subtable)
            (let ((records (cdr subtable)))
              (if (null? records)
                  (error "Empty Table given for lookup" subtable)
                  (let ((sub-records (assoc (car k-list) records)))
                    (if sub-records
                        (iter sub-records (cdr k-list))
                        (error "No record for given key in record: GET" (car k-list) records)))))))
      (iter local-table keys))

    (define (insert! keys value)
      (define (iter subtable k-list)
        (if (null? k-list)
            (set-cdr! subtable value)
            (let ((records (cdr subtable)))
              (if (null? records)
                  (set-cdr! subtable (cons (car (create-record k-list value)) records))
                  (let ((sub-records (assoc (car k-list) records)))
                    (if sub-records
                        (iter sub-records (cdr k-list))
                        (set-cdr! subtable (cons (car (create-record k-list value)) records)))))'ok)))
        (iter local-table keys))

    (define (create-record keys value)
      (if (null? keys)
          value
          (cons (cons (car keys)
                      (create-record (cdr keys)
                                     value))
                '())))
                
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

(define eq-key? equal?)
(define operation-table (make-table eq-key?))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(put '(layer-1 layer-2 letters 1) 'a)
(put '(layer-1 layer-2 letters 2) 'b)
(put '(layer-1 layer-2 letters 3) 'c)
(put '(layer-1 layer-2 characters 1) '!)
(put '(layer-1 layer-2 characters 2) '$)
(put '(layer-1 layer-2 layer-3 numbers 1) 1)
(get '(layer-1 layer-2 letters 3))
