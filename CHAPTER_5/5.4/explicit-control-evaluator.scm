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

(define (self-evaluating? exp) (or (boolean? exp) (number? exp) (string? exp)))
(define (variable? exp) (symbol? exp))
(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))
(define (and? exp) (tagged-list? exp 'and))
(define (or? exp) (tagged-list? exp 'or))
(define (unbind? exp) (tagged-list? exp 'make-unbound!))
(define (let? exp) (tagged-list? exp 'let))
(define (let*? exp) (tagged-list? exp 'let*))
(define (letrec? exp) (tagged-list? exp 'letrec))
(define (assignment? exp) (tagged-list? exp 'set!))
(define (definition? exp) (tagged-list? exp 'define))
(define (if? exp) (tagged-list? exp 'if))
(define (cond? exp) (tagged-list? exp 'cond))
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (application? exp) (pair? exp))
(define (begin? exp) (tagged-list? exp 'begin))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list '> >)
        (list '< <)
        (list '<= <=)
        (list '>= >=)))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define (make-frame variables values)
  (cons variables values))
(define the-empty-environment '())
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (scan vars vals var)
  (cond ((null? vars) false)
        ((eq? var (car vars)) (cons vars vals))
        (else (scan (cdr vars) (cdr vals) var))))

(define (primitive-procedure-names)
  (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let ((frame (first-frame env)))
          (let ((res (scan (frame-variables frame)
                          (frame-values frame)
                          var)))
            (if res
                (set-car! (cdr res) val)
                (env-loop (enclosing-environment env)))))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (let ((res (scan (frame-variables frame) (frame-values frame) var)))
      (if res
          (set-car! (cdr res) val)
          (add-binding-to-frame! var val frame)))))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (let ((res (scan (frame-variables frame)
                           (frame-values frame)
                           var)))
            (if res
                (if (eq? (cadr res) '*unassigned*)
                    (error "Value not yet assigned" (caar res))
                    (cadr res))
                (env-loop (enclosing-environment env)))))))
  (env-loop env))

(define the-global-environment (setup-environment))
(define (get-global-environment) the-global-environment)

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))
(define (apply-primitive-procedure proc args)
  (apply
   (primitive-implementation proc) args))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      #f))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) (cddr exp))))
(define (make-lambda parameters body) (cons 'lambda (cons parameters body)))

(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause) (eq? (cond-predicate clause) 'else))
(define (cond-arrow-clause? clause) (and (not (null? (cdr clause)))
                                         (eq? (cadr clause) '=>)))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond-recipient clause) (caddr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (cond ((cond-else-clause? first)
               (if (null? rest)
                   (sequence->exp (cond-actions first))
                   (error 'expand-clauses "else clause isn't last" clauses)))
              ((cond-arrow-clause? first)
               (make-if (cond-predicate first)
                        (cons (cond-recipient first) (cond-predicate first))
                        (expand-clauses rest)))
              (else (make-if (cond-predicate first)
                             (sequence->exp (cond-actions first))
                             (expand-clauses rest)))))))

(define (let-bindings exp) (cadr exp))
(define (let-body exp) (cddr exp))
(define (let-vars exp)
  (define (iter bindings)
    (if (null? bindings)
        '()
        (cons (caar bindings) (iter (cdr bindings)))))
  (iter (let-bindings exp)))
(define (let-vals exp)
  (define (iter bindings)
    (if (null? bindings)
        '()
        (cons (cadar bindings) (iter (cdr bindings)))))
  (iter (let-bindings exp)))
(define (let->combination exp)
  (if (not (null? (cdr exp)))
      (cons (make-lambda (let-vars exp)
                         (let-body exp))
            (let-vals exp))
      (error 'let->combination "empty let expression" exp)))

(define (empty-arglist) '())
(define (adjoin-arg arg arglist) (append arglist (list arg)))
(define (last-operand? ops) (null? (cdr ops)))
(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))
(define (announce-output string)
  (newline) (display string) (newline))
(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(define eceval
  (make-machine
   (list (list 'prompt-for-input prompt-for-input)
         (list 'read read)
         (list 'announce-output announce-output)
         (list 'get-global-environment get-global-environment)
         (list 'user-print user-print)
         (list 'self-evaluating? self-evaluating?)
         (list 'variable? variable?)
         (list 'quoted? quoted?)
         (list 'assignment? assignment?)
         (list 'definition? definition?)
         (list 'if? if?)
         (list 'lambda? lambda?)
         (list 'begin? begin?)
         (list 'application? application?)
         (list 'lookup-variable-value lookup-variable-value)
         (list 'text-of-quotation text-of-quotation)
         (list 'lambda-body lambda-body)
         (list 'lambda-parameters lambda-parameters)
         (list 'make-procedure make-procedure)
         (list 'operands operands)
         (list 'operator operator)
         (list 'empty-arglist empty-arglist)
         (list 'no-operands? no-operands?)
         (list 'first-operand first-operand)
         (list 'rest-operands rest-operands)
         (list 'adjoin-arg adjoin-arg)
         (list 'last-operand? last-operand?)
         (list 'primitive-procedure? primitive-procedure?)
         (list 'compound-procedure? compound-procedure?)
         (list 'apply-primitive-procedure apply-primitive-procedure)
         (list 'procedure-parameters procedure-parameters)
         (list 'procedure-body procedure-body)
         (list 'procedure-environment procedure-environment)
         (list 'extend-environment extend-environment)
         (list 'begin-actions begin-actions)
         (list 'first-exp first-exp)
         (list 'rest-exps rest-exps)
         (list 'last-exp? last-exp?)
         (list 'if-predicate if-predicate)
         (list 'if-consequent if-consequent)
         (list 'if-alternative if-alternative)
         (list 'true? true?)
         (list 'assignment-variable assignment-variable)
         (list 'assignment-value assignment-value)
         (list 'set-variable-value! set-variable-value!)
         (list 'definition-variable definition-variable)
         (list 'definition-value definition-value)
         (list 'define-variable! define-variable!)
         (list 'cond? cond?)
         (list 'cond->if cond->if)
         (list 'let? let?)
         (list 'let->combination let->combination))
   '(read-eval-print-loop
     (perform (op initialize-stack))
     (perform
      (op prompt-for-input) (const ";;EC-Eval input:"))
     (assign exp (op read))
     (assign env (op get-global-environment))
     (assign continue (label print-result))
     (goto (label eval-dispatch))
     
     print-result
     (perform (op print-stack-statistics)) ; added instruction
     (perform
      (op announce-output) (const ";;; EC-Eval value:"))
     (perform (op announce-output) (const ";;EC-Eval value:"))
     (perform (op user-print) (reg val))
     (goto (label read-eval-print-loop))
     
     unknown-expression-type
     (assign val (const unknown-expression-type-error))
     (goto (label signal-error))
     
     unknown-procedure-type
     (restore continue) ; clean up stack (from apply-dispatch)
     (assign val (const unknown-procedure-type-error))
     (goto (label signal-error))
     
     signal-error
     (perform (op user-print) (reg val))
     (goto (label read-eval-print-loop))
     
     eval-dispatch
     (test (op self-evaluating?) (reg exp))
     (branch (label ev-self-eval))
     (test (op variable?) (reg exp))
     (branch (label ev-variable))
     (test (op quoted?) (reg exp))
     (branch (label ev-quoted))
     (test (op assignment?) (reg exp))
     (branch (label ev-assignment))
     (test (op definition?) (reg exp))
     (branch (label ev-definition))
     (test (op if?) (reg exp))
     (branch (label ev-if))
     (test (op cond?) (reg exp))
     (branch (label ev-cond))
     (test (op let?) (reg exp))
     (branch (label ev-let))
     (test (op lambda?) (reg exp))
     (branch (label ev-lambda))
     (test (op begin?) (reg exp))
     (branch (label ev-begin))
     (test (op application?) (reg exp))
     (branch (label ev-application))
     (goto (label unknown-expression-type))
     
     ev-self-eval
     (assign val (reg exp))
     (goto (reg continue))
     
     ev-variable
     (assign val (op lookup-variable-value) (reg exp) (reg env))
     (goto (reg continue))
     
     ev-quoted
     (assign val (op text-of-quotation) (reg exp))
     (goto (reg continue))
     
     ev-lambda
     (assign unev (op lambda-parameters) (reg exp))
     (assign exp (op lambda-body) (reg exp))
     (assign val (op make-procedure) (reg unev) (reg exp) (reg env))
     (goto (reg continue))
     
     ev-application
     (save continue)
     (save env)
     (assign unev (op operands) (reg exp))
     (save unev)
     (assign exp (op operator) (reg exp))
     (assign continue (label ev-appl-did-operator))
     (goto (label eval-dispatch))
     
     ev-appl-did-operator
     (restore unev) ; the operands
     (restore env)
     (assign argl (op empty-arglist))
     (assign proc (reg val)) ; the operator
     (test (op no-operands?) (reg unev))
     (branch (label apply-dispatch))
     (save proc)
     
     ev-appl-operand-loop
     (save argl)
     (assign exp (op first-operand) (reg unev))
     (test (op last-operand?) (reg unev))
     (branch (label ev-appl-last-arg))
     (save env)
     (save unev)
     (assign continue (label ev-appl-accumulate-arg))
     (goto (label eval-dispatch))
     
     ev-appl-accumulate-arg
     (restore unev)
     (restore env)
     (restore argl)
     (assign argl (op adjoin-arg) (reg val) (reg argl))
     (assign unev (op rest-operands) (reg unev))
     (goto (label ev-appl-operand-loop))
     
     ev-appl-last-arg
     (assign continue (label ev-appl-accum-last-arg))
     (goto (label eval-dispatch))
     
     ev-appl-accum-last-arg
     (restore argl)
     (assign argl (op adjoin-arg) (reg val) (reg argl))
     (restore proc)
     (goto (label apply-dispatch))
     
     apply-dispatch
     (test (op primitive-procedure?) (reg proc))
     (branch (label primitive-apply))
     (test (op compound-procedure?) (reg proc))
     (branch (label compound-apply))
     (goto (label unknown-procedure-type))
     
     primitive-apply
     (assign val (op apply-primitive-procedure)
             (reg proc)
             (reg argl))
     (restore continue)
     (goto (reg continue))
     
     compound-apply
     (assign unev (op procedure-parameters) (reg proc))
     (assign env (op procedure-environment) (reg proc))
     (assign env (op extend-environment)
             (reg unev) (reg argl) (reg env))
     (assign unev (op procedure-body) (reg proc))
     (goto (label ev-sequence))
     
     ev-begin
     (assign unev (op begin-actions) (reg exp))
     (save continue)
     (goto (label ev-sequence))
     
     ev-sequence
     (assign exp (op first-exp) (reg unev))
     (test (op last-exp?) (reg unev))
     (branch (label ev-sequence-last-exp))
     (save unev)
     (save env)
     (assign continue (label ev-sequence-continue))
     (goto (label eval-dispatch))
     
     ev-sequence-continue
     (restore env)
     (restore unev)
     (assign unev (op rest-exps) (reg unev))
     (goto (label ev-sequence))
     
     ev-sequence-last-exp
     (restore continue)
     (goto (label eval-dispatch))
     
     ev-if
     (save exp) ; save expression for later
     (save env)
     (save continue)
     (assign continue (label ev-if-decide))
     (assign exp (op if-predicate) (reg exp))
     (goto (label eval-dispatch)) ; evaluate the predicate
     
     ev-if-decide
     (restore continue)
     (restore env)
     (restore exp)
     (test (op true?) (reg val))
     (branch (label ev-if-consequent))
     
     ev-if-alternative
     (assign exp (op if-alternative) (reg exp))
     (goto (label eval-dispatch))

     ev-cond
     (assign exp (op cond->if) (reg exp))
     (goto (label eval-dispatch))

     ev-let
     (assign exp (op let->combination) (reg exp))
     (goto (label eval-dispatch))
     
     ev-if-consequent
     (assign exp (op if-consequent) (reg exp))
     (goto (label eval-dispatch))
     
     ev-assignment
     (assign unev (op assignment-variable) (reg exp))
     (save unev) ; save variable for later
     (assign exp (op assignment-value) (reg exp))
     (save env)
     (save continue)
     (assign continue (label ev-assignment-1))
     (goto (label eval-dispatch)) ; evaluate the assignment value
     
     ev-assignment-1
     (restore continue)
     (restore env)
     (restore unev)
     (perform
      (op set-variable-value!) (reg unev) (reg val) (reg env))
     (assign val (const ok))
     (goto (reg continue))
     
     ev-definition
     (assign unev (op definition-variable) (reg exp))
     (save unev) ; save variable for later
     (assign exp (op definition-value) (reg exp))
     (save env)
     (save continue)
     (assign continue (label ev-definition-1))
     (goto (label eval-dispatch)) ; evaluate the definition value
     
     ev-definition-1
     (restore continue)
     (restore env)
     (restore unev)
     (perform
      (op define-variable!) (reg unev) (reg val) (reg env))
     (assign val (const ok))
     (goto (reg continue)))))
(define eceval-machine (eceval 'machine))

(define (instructions machine) (machine 'instructions))
(define (stack-registers machine) (machine 'stack-registers))
(define (entry-points machine) (machine 'entry-points))
(define (sources machine) (machine 'sources))
(define (display-line x) (newline) (display x) (newline))

(start eceval-machine)
