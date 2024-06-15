#lang sicp

;(define (eval exp env) ((analyze exp) env))

(define (analyze exp)
  (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((and? exp) (analyze (and->if exp)))
        ((or? exp) (analyze (or->if exp)))
        ((unbind? exp) (analyze-unbound exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp)))
        ((amb? exp) (analyze-amb exp))
        ((let*? exp) (analyze (let*->nested-lets exp)))
        ((letrec? exp) (analyze (letrec->let exp)))
        ((application? exp) (analyze-application exp))
        (else (error "Unknown expression type: ANALYZE" exp))))

#|(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((and? exp) (eval-and exp env))
        ((or? exp) (eval-or exp env))
        ((unbind? exp) (make-unbound exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((let? exp) (eval (let->combination exp) env))
        ((let*? exp) (eval (let*->nested-lets exp) env))
        ((letrec? exp) (display (letrec->let exp)) (eval (letrec->let exp) env))
        ((application? exp)
         (new-apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type: EVAL" exp))))|#

#|(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment
           (procedure-parameters proc)
           args
           (procedure-environment proc))))
        (else
         (error "Unknown procedure type: EXECUTE-APPLICATION"
                proc))))|#

(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args)
                  fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment
           (procedure-parameters proc)
           args
           (procedure-environment proc))
          succeed
          fail))
        (else
         (error "Unknown procedure type: EXECUTE-APPLICATION"
                proc))))

#|(define (new-apply proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         (eval-sequence
          (procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))))
        (else (error 'new-apply "unknown procedure type" proc))))|#


(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))

(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices)
             env
             succeed
             (lambda () (try-next (cdr choices))))))
      (try-next cprocs))))

(define (self-evaluating? exp) (or (boolean? exp) (number? exp) (string? exp)))
(define (variable? exp) (symbol? exp))
(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))
(define (tagged-list? exp tag) (and (pair? exp) (eq? (car exp) tag)))
(define (and? exp) (tagged-list? exp 'and))
(define (or? exp) (tagged-list? exp 'or))
(define (unbind? exp) (tagged-list? exp 'make-unbound!))
(define (let? exp) (tagged-list? exp 'let))
(define (let*? exp) (tagged-list? exp 'let*))
(define (letrec? exp) (tagged-list? exp 'letrec))
(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))
(define (make-assignment var val) (list 'set! var val))
(define (make-quote val) (list 'quote val))

#|(define (analyze-self-evaluating exp)
  (lambda (env) exp))|#

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

#|(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))|#

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))

#|(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))|#

(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env) fail)))

(define (analyze-unbound exp)
  (lambda (env) (make-unbound exp env)))

#|(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))|#

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2) ; *1*
               (let ((old-value
                      (lookup-variable-value var env)))
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lambda () ; *2*
                            (set-variable-value!
                             var old-value env)
                            (fail2)))))
             fail))))

#|(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))|#

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (define-variable! var val env)
               (succeed 'ok fail2))
             fail))))

#|(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env) (if (true? (pproc env))
                      (cproc env)
                      (aproc env)))))|#

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             ;; success continuation for evaluating the predicate
             ;; to obtain pred-value
             (lambda (pred-value fail2)
               (if (true? pred-value)
                   (cproc env succeed fail2)
                   (aproc env succeed fail2)))
             ;; failure continuation for evaluating the predicate
             fail))))

#|(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (scan-out-defines (lambda-body exp)))))
    (lambda (env) (make-procedure vars bproc env))))|#

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (scan-out-defines (lambda-body exp)))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env) fail))))

#|(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs) (error "Empty sequence: ANALYZE"))
    (loop (car procs) (cdr procs))))|#

(define (analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
         ;; success continuation for calling a
         (lambda (a-value fail2)
           (b env succeed fail2))
         ;; failure continuation for calling a
         fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc
                            (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence: ANALYZE"))
    (loop (car procs) (cdr procs))))

#|(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application
       (fproc env)
       (map (lambda (aproc) (aproc env))
            aprocs)))))|#

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
             (lambda (proc fail2)
               (get-args aprocs
                         env
                         (lambda (args fail3)
                           (execute-application
                            proc args succeed fail3))
                         fail2))
             fail))))

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs)
       env
       ;; success continuation for this aproc
       (lambda (arg fail2)
         (get-args
          (cdr aprocs)
          env
          ;; success continuation for
          ;; recursive call to get-args
          (lambda (args fail3)
            (succeed (cons arg args) fail3))
          fail2))
       fail)))

#|(define (eval-and exp env)
  (cond ((no-pred? exp) 'true)
        ((true? (eval (first-pred exp) env)) (eval-and (rest-pred exp) env))
        (else (eval (first-pred exp) env))))

(define (eval-or exp env)
  (cond ((no-pred? exp) 'false)
        ((not (true? (eval (first-pred exp) env))) (eval-and (rest-pred exp) env))
        (else (eval (first-pred exp) env))))|#

(define (and->if exp)
  (expand-preds (preds exp) 'true))
(define (or->if exp)
  (expand-preds (preds exp) 'false))

(define (expand-preds preds def)
  (if (null? preds)
      def
      (let ((first (car preds))
            (rest (cdr preds)))
        (if (eq? def 'true)
            (make-if first
                     (expand-preds rest def)
                     'false)
            (make-if first
                     'true
                     (expand-preds rest def))))))
      
(define (preds exp) (cdr exp))
(define (no-pred? exp)
  (null? (preds exp)))
(define (first-pred exp)
  (car (preds exp)))
(define (rest-pred exp)
  (cons (car exp) (cdr (preds exp))))
      
(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))

#|(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env))

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env))|#

(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) (cddr exp))))
(define (make-definition variable value) (list 'define variable value))
(define (make-lambda-definition name parameters body)
  (cons 'define (cons (cons name parameters) body)))
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body) (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      #f))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))
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
(define (make-let bindings body)
  (cons 'let (cons bindings body)))

(define (let->combination exp)
  (if (not (null? (cdr exp)))
      (cons (make-lambda (let-vars exp)
                         (let-body exp))
            (let-vals exp))
      (error 'let->combination "empty let expression" exp)))

(define (let*-bindings exp) (cadr exp))
(define (let*-body exp) (caddr exp))
(define (let*->nested-lets exp) (make-nested-lets (let-bindings exp) (let-body exp)))

(define (make-nested-lets bindings body)
  (if (null? bindings)
      body
      (make-let (list (car bindings))
                (let ((res (make-nested-lets (cdr bindings)
                                             body)))
                  (if (let? res)
                      (list res)
                      res)))))

(define (scan-out-defines proc-body)
  (define (seperate-def-acts body defs)
    (let ((first (car body)))
      (if (not (definition? first))
          (cons defs body)
          (seperate-def-acts (cdr body) (cons first defs)))))
  (define (def->binds-assigns defs binds assigns)
    (if (null? defs)
        (cons binds assigns)
        (let ((first (car defs)))
          (def->binds-assigns (cdr defs)
            (cons (list (definition-variable first)
                        (make-quote '*unassigned*))
                  binds)
            (cons (make-assignment (definition-variable first)
                                   (definition-value first))
                  assigns)))))
  (let* ((def-actions (seperate-def-acts proc-body '()))
         (defs (car def-actions))
         (actions (cdr def-actions))
         (bindings-assigns (def->binds-assigns defs '() '()))
         (bindings (car bindings-assigns))
         (assigns (cdr bindings-assigns))
         (body (append assigns actions)))
    (if (null? assigns)
        proc-body
        (list (make-let bindings body)))))

(define (letrec->let exp)
  (let ((binds (let-bindings exp))
        (body (let-body exp)))
    (make-let (map (lambda (bind)
                     (list (car bind) (make-quote '*unassigned*))) binds)
              (append (map (lambda (bind)
                             (make-assignment (car bind) (cadr bind))) binds) body))))
  
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define (set-first-frame! env new-frame) (set-car! env new-frame))
(define the-empty-environment '())
(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (scan vars vals var)
  (cond ((null? vars) false)
        ((eq? var (car vars)) (cons vars vals))
        (else (scan (cdr vars) (cdr vals) var))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (let ((res (scan (frame-variables frame)
                           (frame-values frame)
                           var)))
            (if res
                (cadr res)
                #|(if (eq? (cadr res) '*unassigned*)
                    (error "Value not yet assigned" (caar res))
                    (cadr res))|#
                (env-loop (enclosing-environment env)))))))
  (env-loop env))

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

(define (make-unbound exp env)
  (unbind-variable! (cadr exp) env))

(define (unbind-variable! var env)
  (define (make-new-frame old-vars old-vals new-vars new-vals)
    (cond ((null? old-vars) (make-frame new-vars new-vals))
          ((eq? var (car old-vars)) (make-new-frame (cdr old-vars)
                                                    (cdr old-vals)
                                                    new-vars
                                                    new-vals))
          (else (make-new-frame (cdr old-vars)
                                (cdr old-vals)
                                (cons (car old-vars) new-vars)
                                (cons (car old-vals) new-vals)))))
  (let* ((frame (first-frame env))
         (new-frame (make-new-frame (frame-variables frame) (frame-values frame) '() '())))
    (set-first-frame! env new-frame)
    'ok))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list 'not not)
        (list 'abs abs)
        (list 'member member)
        (list 'memq memq)
        (list '= =)
        (list '> >)
        (list '< <)
        (list '<= <=)
        (list '>= >=)
        (list 'list list)))
(define (primitive-procedure-names)
  (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))
(define (apply-primitive-procedure proc args)
  (apply
   (primitive-implementation proc) args))
(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))

(define input-prompt ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval value:")
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

#|(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))|#

(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
          (try-again)
          (begin
            (newline) (display ";;; Starting a new problem ")
            (ambeval
             input
             the-global-environment
             ;; ambeval success
             (lambda (val next-alternative)
               (announce-output output-prompt)
               (user-print val)
               (internal-loop next-alternative))
             ;; ambeval failure
             (lambda ()
               (announce-output
                ";;; There are no more values of")
               (user-print input)
               (driver-loop)))))))
  (ambeval '(begin (define (require p) (if (not p) (amb)))

             (define nouns '(noun student professor cat class))
             (define verbs '(verb studies lectures eats sleeps))
             (define articles '(article the a))
             (define prepositions '(prep for to in by with))
             (define adjectives '(adjective wise fool sane crazy hot black))
             (define adverbs '(adverb reluctantly noisily quickly loudly always often seldom never))

             (define *unparsed* '())

             (define (parse-sentence)
               (list 'sentence (parse-noun-phrase) (parse-verb-phrase)))

             (define (parse-simple-verb-phrase)
              (amb (list 'simple-verb-phrase
                         (parse-word verbs))
                   (list 'simple-verb-phrase
                         (parse-word adverbs)
                         (parse-word verbs))))
             
             (define (parse-verb-phrase)
               (define (maybe-extend verb-phrase)
                 (amb (list verb-phrase)
                      (maybe-extend
                       (list 'verb-phrase
                             verb-phrase
                             (parse-prepositional-phrase)))))
               (maybe-extend (parse-simple-verb-phrase)))

             (define (parse-simple-noun-phrase)
               (list 'simple-noun-phrase
                     (parse-word articles)
                     (parse-word adjectives)
                     (parse-word nouns)))

             (define (parse-noun-phrase)
               (define (maybe-extend noun-phrase)
                 (amb noun-phrase
                      (maybe-extend
                       (list 'noun-phrase
                             noun-phrase
                             (parse-prepositional-phrase)))))
               (maybe-extend (parse-simple-noun-phrase)))

             (define (parse-word word-list)
               (require (not (null? *unparsed*)))
               (require (memq (car *unparsed*) (cdr word-list)))
               (let ((found-word (car *unparsed*)))
                 (set! *unparsed* (cdr *unparsed*))
                 (list (car word-list) found-word)))

             (define (parse-prepositional-phrase)
               (list 'prep-phrase
                     (parse-word prepositions)
                     (parse-noun-phrase)))

             (define (parse input)
               (set! *unparsed* input)
               (let ((sent (parse-sentence)))
                 (require (null? *unparsed*)) sent)))
           
           the-global-environment
           
           (lambda (val fail) (internal-loop
                       (lambda ()
                         (newline) (display ";;; There is no current problem")
                         (driver-loop))))
           (lambda () (error "Error in grammar"))))

(define the-global-environment (setup-environment))
(driver-loop)