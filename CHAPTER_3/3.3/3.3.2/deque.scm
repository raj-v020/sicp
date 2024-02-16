#lang sicp

(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))

(define (cnode a b c)
  (cons (cons b a) c))

(define (prev node)
  (cdar node))
(define (next node)
  (cdr node))
(define (data node)
  (caar node))

(define (set-prev! node item)
  (set-cdr! (car node) item))

(define (set-next! node item)
  (set-cdr! node item))

(define (set-data! node item)
  (set-car! (car node) item))

(define (print-node node)
    (define (make-l aux-node aux-list)
      (if (null? aux-node)
          aux-list
          (make-l (cdr aux-node) (append aux-list (cons (data aux-node) '())))))
  (make-l node '()))

(define (make-deque)
  (let ((front-ptr '())
        (rear-ptr '()))

    (define (set-front-ptr! item)
      (set! front-ptr item))
    (define (set-rear-ptr! item)
      (set! rear-ptr item))
    
    (define (insert-front item)
      (let ((new-node (cnode '() item '())))
        (cond ((empty?) (set-front-ptr! new-node)
                        (set-rear-ptr! new-node))
              (else (set-next! new-node front-ptr)
                    (set-prev! front-ptr new-node)
                    (set-front-ptr! new-node)))
        (print)))

    (define (delete-front)
      (cond ((empty?) (error "DELETE-FRONT called with empty deque" (print)))
            (else (set-front-ptr! (next front-ptr))
                  (set-prev! front-ptr '())
                  (print))))

    (define (insert-rear item)
      (let ((new-node (cnode '() item '())))
        (cond ((empty?) (set-front-ptr! new-node)
                        (set-rear-ptr! new-node))
              (else (set-prev! new-node rear-ptr)
                    (set-next! rear-ptr new-node)
                    (set-rear-ptr! new-node)))
        (print)))

    (define (delete-rear)
      (cond ((empty?) (error "DELETE-REAR called with empty deque" (print)))
            (else (set-next! (prev rear-ptr) '())
                  (set-rear-ptr! (prev rear-ptr))
                  (print))))
              
    (define (empty?) (null? front-ptr))
    (define (print) (print-node front-ptr))
    (define (dispatch m)
      (cond ((eq? m 'empty?) empty?)
            ((eq? m 'insert-front) insert-front)
            ((eq? m 'delete-front) delete-front)
            ((eq? m 'insert-rear) insert-rear)
            ((eq? m 'delete-rear) delete-rear)
            ((eq? m 'front-ptr) front-ptr)
            ((eq? m 'rear-ptr) rear-ptr)
            ((eq? m 'print) print)
            (else (error "QUEUE wrong dispatch message, MESSAGE: " m))))
    dispatch))


(define d1 (make-deque))

((d1 'insert-front) 'b)
((d1 'insert-front) 'a)
((d1 'insert-rear) 'c)
((d1 'delete-front))
((d1 'delete-rear))


