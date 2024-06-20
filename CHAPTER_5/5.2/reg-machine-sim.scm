#lang sicp

(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (remove s seq)
  (filter (lambda (x) (not (equal? x s)))
          seq))

(define (make-machine ops controller-text)
  (let ((machine (make-new-machine ops))
        (insts '())
        (entry-points '())
        (stack-regs '())
        (sources '()))
    
    (for-each (lambda (inst)
                (if (pair? inst)
                    (begin (if (not (member (car inst) insts))
                               (set! insts (cons (car inst) insts)))
                           (cond ((eq? (car inst) 'goto) (if (pair? (cdr inst)) 
                                                             (if (and (not (member (cadr inst) entry-points))
                                                                      (eq? (caadr inst) 'reg)) (set! entry-points (cons (cadr inst) entry-points)))))
                                 ((or (eq? (car inst) 'save)
                                      (eq? (car inst) 'restore)) (if (not (member (cadr inst) stack-regs)) (set! stack-regs (cons (cadr inst) stack-regs))))
                                 ((eq? (car inst) 'assign) (set! sources (cons (cons (cadr inst) (cddr inst)) sources))))))) controller-text)

    (define (get-labels text)
      (cond ((null? text) '())
            ((not (pair? (car text))) (if (null? (cdr text))
                                          (cons (cons '() (car text))
                                                (get-labels (cdr text)))
                                          (cons (cons (cadr text) (car text))
                                                (get-labels (cddr text)))))
            (else (get-labels (cdr text)))))

    ((machine 'install-operations) ops)
    ((machine 'install-labels) (get-labels controller-text))
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine (machine 'allocate-register)))
    
    (lambda (m)
      (cond ((eq? m 'machine) machine)
            ((eq? m 'instructions) insts)
            ((eq? m 'stack-registers) stack-regs)
            ((eq? m 'entry-points) entry-points)
            ((eq? m 'sources) sources)
            (else (error "Unknown message to make-machine" m))))))

(define (make-register name)
  (let ((contents '*unassigned*)
        (trace false))
    
    (define (print-reg val)
      (newline) (display "Register: ") (display name) (newline)
      (display "Old Value: ") (display contents) (newline)
      (display "New Value: ") (display val) (newline))
    
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value) (if trace (print-reg value)) (set! contents value)))
            ((eq? message 'trace-on) (set! trace true))
            ((eq? message 'trace-off) (set! trace false))
            (else
             (error "Unknown request: REGISTER" message))))
    dispatch))

(define (get-contents register) (register 'get))
(define (set-contents! register value)
  ((register 'set) value))

(define (make-stack)
  (let ((s '())
        (number-pushes 0)
        (max-depth 0)
        (current-depth 0))
    
    (define (push x)
      (set! number-pushes (+ 1 number-pushes))
      (set! current-depth (+ 1 current-depth))
      (set! max-depth (max current-depth max-depth))
      (set! s (cons x s)))
  
  (define (pop)
      (if (null? s)
          (error "Empty stack: POP")
          (let ((top (car s)))
            (set! s (cdr s))
            (set! current-depth (- current-depth 1))
            top)))
  
  (define (initialize)
    (set! s '())
    (set! number-pushes 0)
    (set! max-depth 0)
    (set! current-depth 0)
    'done)
  
  (define (print-statistics)
      (display-line (list 'total-pushes '= number-pushes
                     'maximum-depth '= max-depth)))
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            ((eq? message 'print-statistics)
             (print-statistics))
            (else (error "Unknown request: STACK" message))))
    dispatch))

(define (pop stack) (stack 'pop))
(define (push stack value) ((stack 'push) value))

(define (make-new-machine ops)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag)))
          (labels '())
          (breakpoint-label 0)
          (breakpoints '())
          (instruction-count 0)
          (trace false)
          (count (cons 0 -1)))
      (set! ops (append the-ops ops))
      (define (allocate-register name)
        (if (not (assoc name register-table))
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register:" name))))
      
      (define (print-instruction inst)
        (display "Instruction no. ") (display instruction-count) (display ": ")
        (if (assoc (car inst) labels) (display (cdr (assoc (car inst) labels)))) (display " ")
        (display (car inst)) (newline))
      
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (let ((inst (car insts)))
                (let ((label (assoc (car inst) labels)))
                  (if label (let ((breakpoint (assoc (cdr label) breakpoints)))
                              (if breakpoint
                                  (begin (set-cdr! count (cadr breakpoint)) (set! breakpoint-label (car breakpoint)))))))
                (if (> (cdr count) 0) 
                    (if (not (= (cdr count) (car count)))
                        (begin (if trace (print-instruction inst))
                                ((instruction-execution-proc inst))
                                (set! instruction-count (+ instruction-count 1))
                                (set-car! count (+ 1 (car count)))
                                (execute))
                        (begin (newline) (display "Label: ") (display breakpoint-label) (newline)
                               (display "Offset: ") (display (car count)) (newline)))
                    (begin (if trace (print-instruction inst))
                                ((instruction-execution-proc inst))
                                (set! instruction-count (+ instruction-count 1))
                                (execute)))))))
      
      (define (print-instruction-count)
        (display "No. of Instructions performed: ") (display instruction-count) (newline))
      
      (define (reset-instruction-count)
        (set! instruction-count 0))

      (define (set-reg-trace! name rtrace)
        (let ((reg (assoc name register-table)))
          (if reg
              (if rtrace ((cadr reg) 'trace-on)
                  ((cadr reg) 'trace-off))
              (error "Register not found: SET-REG-TRACE!" name))))

      (define (set-breakpoint name num)
        (set! breakpoints (append (list (list name num)) breakpoints)))
      (define (cancel-breakpoint name num)
        (set! breakpoints (remove (list name num) breakpoints)))
      (define (cancel-all-breakpoints)
        (set! breakpoints '()))
      
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq)
                 (set! the-instruction-sequence seq)))
              ((eq? message 'install-labels) (lambda (val)
                                                           (set! labels val)))
              ((eq? message 'allocate-register)
               allocate-register)
              ((eq? message 'get-register)
               lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops)
                 (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              ((eq? message 'print-ic) (print-instruction-count))
              ((eq? message 'reset-ic) (reset-instruction-count))
              ((eq? message 'itrace-on) (set! trace true))
              ((eq? message 'itrace-off) (set! trace false))
              ((eq? message 'rtrace-on) (lambda (reg-name) (set-reg-trace! reg-name true)))
              ((eq? message 'rtrace-off) (lambda (reg-name) (set-reg-trace! reg-name false)))
              ((eq? message 'set-breakpoint) set-breakpoint)
              ((eq? message 'cancel-breakpoint) cancel-breakpoint)
              ((eq? message 'cancel-all-breakpoints) (cancel-all-breakpoints))
              ((eq? message 'proceed) (set! count (cons 0 -1)) (execute))
              (else (error "Unknown request: MACHINE"
                           message))))
      dispatch)))

(define (assemble controller-text machine alloc-reg)
  (extract-labels
   controller-text
   alloc-reg
   (lambda (insts labels)
     (update-insts! insts labels machine)
     insts)))

(define (extract-labels text alloc-reg receive)
  (if (null? text)
      (receive '() '())
      (extract-labels
       (cdr text)
       alloc-reg
       (lambda (insts labels)
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
               (let ((label (make-label-entry next-inst
                                              insts)))
                 (if (member label labels)
                     (error "Multiple lables with same name not allowed" next-inst)
                     (receive insts
                              (cons label
                                    labels))))
               (begin (if (eq? (car next-inst) 'assign)
                          (alloc-reg (cadr next-inst)))
               (receive (cons (make-instruction next-inst)
                              insts)
                        labels))))))))

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc!
        inst
        (make-execution-procedure
         (instruction-text inst)
         labels machine pc flag stack ops)))
     insts)))

(define (make-instruction text) (cons text '()))
(define (instruction-text inst) (car inst))
(define (instruction-execution-proc inst) (cdr inst))
(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst proc))

(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        (cdr val)
        (error "Undefined label: ASSEMBLE"
               label-name))))

(define (make-execution-procedure
         inst labels machine pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        (else
         (error "Unknown instruction type: ASSEMBLE"
                inst))))

(define (make-assign inst machine labels operations pc)
  (let ((target
         (get-register machine (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    (let ((value-proc
           (if (operation-exp? value-exp)
               (make-operation-exp
                value-exp machine labels operations)
               (make-primitive-exp
                (car value-exp) machine labels))))
      (lambda () ; execution procedure for assign
        (set-contents! target (value-proc))
        (advance-pc pc)))))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))
(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
        (let ((condition-proc
               (make-operation-exp
                condition machine labels operations)))
          (lambda ()
            (set-contents! flag (condition-proc))
            (advance-pc pc)))
        (error "Bad TEST instruction: ASSEMBLE" inst))))

(define (test-condition test-instruction)
  (cdr test-instruction))

(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
        (let ((insts
               (lookup-label
                labels
                (label-exp-label dest))))
          (lambda ()
            (if (get-contents flag)
                (set-contents! pc insts)
                (advance-pc pc))))
        (error "Bad BRANCH instruction: ASSEMBLE" inst))))

(define (branch-dest branch-instruction)
  (cadr branch-instruction))

(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts (lookup-label
                         labels
                         (label-exp-label dest))))
             (lambda () (set-contents! pc insts))))
          ((register-exp? dest)
           (let ((reg (get-register
                       machine
                       (register-exp-reg dest))))
             (lambda ()
               (set-contents! pc (get-contents reg)))))
          (else (error "Bad GOTO instruction: ASSEMBLE" inst)))))

(define (goto-dest goto-instruction)
  (cadr goto-instruction))

(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))

(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
        (let ((action-proc
               (make-operation-exp
                action machine labels operations)))
          (lambda () (action-proc) (advance-pc pc)))
        (error "Bad PERFORM instruction: ASSEMBLE" inst))))

(define (perform-action inst) (cdr inst))

(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ((label-exp? exp)
         (let ((insts (lookup-label
                       labels
                       (label-exp-label exp))))
           (lambda () insts)))
        ((register-exp? exp)
         (let ((r (get-register machine (register-exp-reg exp))))
           (lambda () (get-contents r))))
        (else (error "Unknown expression type: ASSEMBLE" exp))))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (tag-exp exp) tag)
      false))
(define (tag-exp exp) (car exp))
(define (register-exp? exp) (tagged-list? exp 'reg))
(define (register-exp-reg exp) (cadr exp))
(define (constant-exp? exp) (tagged-list? exp 'const))
(define (constant-exp-value exp) (cadr exp))
(define (label-exp? exp) (tagged-list? exp 'label))
(define (label-exp-label exp) (cadr exp))

(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp)
                         operations)))
    (if (memq 'label (map tag-exp (operation-exp-operands exp)))
        (error "Given LABEL as Input for Operation")
        (let ((aprocs
               (map (lambda (e)
                      (make-primitive-exp e machine labels))
                    (operation-exp-operands exp))))
          (lambda ()
            (apply op (map (lambda (p) (p)) aprocs)))))))

(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))
(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))
(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
        (cadr val)
        (error "Unknown operation: ASSEMBLE"
               symbol))))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

(define (start machine) (machine 'start))
(define (print-ic machine) (machine 'print-ic))
(define (reset-ic machine) (machine 'reset-ic))

(define (instruction-trace machine message) (cond ((eq? message 'on) (machine 'itrace-on))
                                                  ((eq? message 'off) (machine 'itrace-off))
                                                  (else "Invalid message: TRACE" message)))

(define (register-trace machine reg-names message) (for-each (lambda (reg-name)
                                                               (cond ((eq? message 'on) ((machine 'rtrace-on) reg-name))
                                                                     ((eq? message 'off) ((machine 'rtrace-off) reg-name))
                                                                     (else "Invalid message: TRACE" message)))
                                                             reg-names))
(define (set-breakpoint machine label n)
  ((machine 'set-breakpoint) label n))

(define (cancel-breakpoint machine label n)
  ((machine 'cancel-breakpoint) label n))

(define (proceed machine)
  (machine 'proceed))

(define (cancel-all-breakpoint machine label n)
  (machine 'cancel-all-breakpoint))
(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))
(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name)
                 value)
  'done)

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

(define (instructions machine) (machine 'instructions))
(define (stack-registers machine) (machine 'stack-registers))
(define (entry-points machine) (machine 'entry-points))
(define (sources machine) (machine 'sources))
(define (display-line x) (newline) (display x) (newline))

(define gcd
  (make-machine
   (list (list 'rem remainder)
         (list '= =)
         (list 'read read)
         (list 'print display-line))
   '(gcd-loop
     (assign a (op read))
     (assign b (op read))
     test-b
     (test (op =) (reg b) (const 0))
            (branch (label gcd-done))
            (assign t (op rem) (reg a) (reg b))
            (assign a (reg b))
            (assign b (reg t))
            (goto (label test-b))
            gcd-done
            (perform (op print)
                     (reg a))
            (goto (label gcd-loop)))))
(define gcd-machine (gcd 'machine))

#|(display-line (instructions gcd))
(display-line (stack-registers gcd))
(display-line (entry-points gcd))
(display-line (sources gcd))

(set-register-contents! gcd-machine 'a 13)
(set-register-contents! gcd-machine 'b 12)
(start gcd-machine)
(get-register-contents gcd-machine 'a)
(get-register-contents gcd-machine 'b)|#

(define fib
  (make-machine
   (list (list '< <)
         (list '- -)
         (list '+ +))
   '((assign continue (label fib-done))
     fib-loop
     (test (op <) (reg n) (const 2))
     (branch (label immediate-answer))
     ;; set up to compute Fib(n  1)
     (save continue)
     (assign continue (label afterfib-n-1))
     (save n) ; save old value of n
     (assign n (op -) (reg n) (const 1)) ; clobber n to n-1
     (goto (label fib-loop)) ; perform recursive call
     afterfib-n-1 ; upon return, val contains Fib(n  1)
     (restore n)
     (restore continue)
     ;; set up to compute Fib(n  2)
     (assign n (op -) (reg n) (const 2))
     (save continue)
     (assign continue (label afterfib-n-2))
     (save val) ; save Fib(n  1)
     (goto (label fib-loop))
     afterfib-n-2 ; upon return, val contains Fib(n  2)
     (assign n (reg val)) ; n now contains Fib(n  2)
     (restore val) ; val now contains Fib(n  1)
     (restore continue)
     (assign val ; Fib(n  1) + Fib(n  2)
             (op +) (reg val) (reg n))
     (goto (reg continue)) ; return to caller, answer is in
     val
     immediate-answer
     (assign val (reg n)) ; base case: Fib(n) = n
     (goto (reg continue))
     fib-done)))
(define fib-machine (fib 'machine))

#|(display-line (instructions fib))
(display-line (stack-registers fib))
(display-line (entry-points fib))
(display-line (sources fib))

(set-register-contents! fib-machine 'n 5)
(start fib-machine)
(get-register-contents fib-machine 'val)|#

(define factorial
  (make-machine
   (list (list '- -)
         (list '* *)
         (list '= =)
         (list 'read read)
         (list 'print display-line))
   '(loop
     (assign n (op read))
     (assign continue (label fact-done)) ;set up final return address
     fact-loop
     (test (op =) (reg n) (const 1))
     (branch (label base-case))
     ;; Set up for the recursive call by saving n and continue.
     ;; Set up continue so that the computation will continue
     ;; at after-fact when the subroutine returns.
     (save continue)
     (save n)
     (assign n (op -) (reg n) (const 1))
     (assign continue (label after-fact))
     (goto (label fact-loop))
     after-fact
     (restore n)
     (restore continue)
     (assign val (op *) (reg n) (reg val)) ;val now contains n(n - 1)!
     (goto (reg continue)) ;return to caller
     base-case
     (assign val (const 1)) ;base case: 1! = 1
     (goto (reg continue)) ;return to caller
     fact-done
     (perform (op print)
              (reg val))
     (perform (op print-stack-statistics))
     (perform (op initialize-stack))
     (goto (label loop)))))
(define factorial-machine (factorial 'machine))
;(instruction-trace factorial-machine 'on)
;(set-breakpoint factorial-machine 'after-fact 4)
;(cancel-breakpoint factorial-machine 'after-fact 4)


#|(display-line (instructions factorial))
(display-line (stack-registers factorial))
(display-line (entry-points factorial))
(display-line (sources factorial))|#

#|(start factorial-machine)
(get-register-contents factorial-machine 'val)
(proceed factorial-machine)
(get-register-contents factorial-machine 'val)
(proceed factorial-machine)|#
