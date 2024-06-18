#lang sicp

(define (make-table same-key?)
  (let ((table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable
             (assoc key-1 (cdr table))))
        (if subtable
            (let ((record
                   (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))

    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr table)))))
      'ok)
                
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

(define eq-key? equal?)
(define operation-table (make-table eq-key?))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(define (tagged-list? exp tag) (and (pair? exp) (eq? (car exp) tag)))

(define stream-car car)
(define (stream-cdr stream) (force (cdr stream)))

(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (remove s seq)
  (filter (lambda (x) (not (equal? x s)))
          seq))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x) (newline) (display x))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter
                       pred
                       (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-append s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (stream-append (stream-cdr s1) s2))))

(define (stream-append-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream
       (stream-car s1)
       (stream-append-delayed
        (stream-cdr s1)
        delayed-s2))))

(define (interleave-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream
       (stream-car s1)
       (interleave-delayed
        (force delayed-s2)
        (delay (stream-cdr s1))))))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream
       (stream-car s1)
       (interleave
        s2
        (stream-cdr s1)))))

(define (stream-flatmap proc s)
  (flatten-stream (stream-map proc s)))

(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave-delayed
       (stream-car stream)
       (delay (flatten-stream (stream-cdr stream))))))

(define (singleton-stream x)
  (cons-stream x the-empty-stream))

(define (qeval query frame-stream)
  (let ((qproc (get (type query) 'qeval)))
    (if qproc
        (qproc (contents query) frame-stream)
        (simple-query query frame-stream))))

(define (type exp)
  (if (pair? exp)
      (car exp)
      (error "Unknown expression TYPE" exp)))

(define (contents exp)
  (if (pair? exp)
      (cdr exp)
      (error "Unknown expression CONTENTS" exp)))

(define (simple-query query-pattern frame-stream)
  (stream-flatmap
   (lambda (frame)
     (stream-append-delayed
      (find-assertions query-pattern frame)
      (delay (apply-rules query-pattern frame))))
   frame-stream))

(define (find-assertions pattern frame)
  (stream-flatmap
   (lambda (datum) (check-an-assertion datum pattern frame))
   (fetch-assertions pattern frame)))

(define (check-an-assertion assertion query-pat query-frame)
  (let ((match-result
         (pattern-match query-pat assertion query-frame)))
    (if (eq? match-result 'failed)
        the-empty-stream
        (singleton-stream match-result))))

(define (pattern-match pat dat frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? pat dat) frame)
        ((var? pat) (extend-if-consistent pat dat frame))
        ((and (pair? pat) (pair? dat))
         (pattern-match
          (cdr pat)
          (cdr dat)
          (pattern-match (car pat) (car dat) frame)))
        (else 'failed)))

(define (extend-if-consistent var dat frame)
  (let ((binding (binding-in-frame var frame)))
    (if binding
        (pattern-match (binding-value binding) dat frame)
        (extend var dat frame))))

(define (apply-rules pattern frame)
  (stream-flatmap (lambda (rule)
                    (apply-a-rule rule pattern frame))
                  (fetch-rules pattern frame)))

(define running-queries '())
(define (is-running? query) (member query running-queries))
(define (add-running! query) (set! running-queries (cons query running-queries)))
(define (remove-running! query) (set! running-queries (remove query running-queries)))
(define (reset-running-queries) (set! running-queries '()))

(define (canonical-name var)
  (if (number? (cadr var))
      (list (car var) (caddr var))
      var))

(define (apply-a-rule rule query-pattern query-frame)
  ;(display "RQ: ")(display running-queries) (newline) (newline)
  (let ((clean-rule (rename-variables-in rule)))
    (let ((unify-result (unify-match query-pattern
                                     (conclusion clean-rule)
                                     query-frame)))
      (if (eq? unify-result 'failed)
          the-empty-stream
          (let ((query (instantiate (conclusion clean-rule) unify-result (lambda (v f)
                   (canonical-name v)))))
            (if (is-running? query)
                the-empty-stream
                (begin (add-running! query)
                       (qeval (rule-body clean-rule)
                                     (singleton-stream unify-result)))))))))

                     ;  (display (rule-body clean-rule)) (newline)
                   ;(display "RB: ") (display (instantiate (rule-body clean-rule) unify-result (lambda (v f) (canonical-name v)))) (newline)
(define (rename-variables-in rule)
  (let ((rule-application-id (new-rule-application-id)))
    (define (tree-walk exp)
      (cond ((var? exp)
             (make-new-variable exp rule-application-id))
            ((pair? exp)
             (cons (tree-walk (car exp))
                   (tree-walk (cdr exp))))
            (else exp)))
    (tree-walk rule)))

(define rule-counter 0)

(define (new-rule-application-id)
  (set! rule-counter (+ 1 rule-counter))
  rule-counter)

(define (make-new-variable var rule-application-id)
  (cons '? (cons rule-application-id (cdr var))))

(define (unify-match p1 p2 frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? p1 p2) frame)
        ((var? p1) (extend-if-possible p1 p2 frame))
        ((var? p2) (extend-if-possible p2 p1 frame)) ; ***
        ((and (pair? p1) (pair? p2))
         (unify-match (cdr p1)
                      (cdr p2)
                      (unify-match (car p1)
                                   (car p2)
                                   frame)))
        (else 'failed)))

(define (extend-if-possible var val frame)
  (let ((binding (binding-in-frame var frame)))
    (cond (binding
           (unify-match (binding-value binding) val frame))
          ((var? val) ; ***
           (let ((binding (binding-in-frame val frame)))
             (if binding
                 (unify-match
                  var (binding-value binding) frame)
                 (extend var val frame))))
          ((depends-on? val var frame) ; ***
           'failed)
          (else (extend var val frame)))))

(define (depends-on? exp var frame)
  (define (tree-walk e)
    (cond ((var? e)
           (if (equal? var e)
               true
               (let ((b (binding-in-frame e frame)))
                 (if b
                     (tree-walk (binding-value b))
                     false))))
          ((pair? e)
           (or (tree-walk (car e))
               (tree-walk (cdr e))))
          (else false)))
  (tree-walk exp))

(define THE-ASSERTIONS the-empty-stream)

(define (fetch-assertions pattern frame)
  (if (use-index? pattern)
      (get-indexed-assertions pattern)
      (get-all-assertions)))

(define (get-all-assertions) THE-ASSERTIONS)

(define (get-indexed-assertions pattern)
  (get-stream (index-key-of pattern) 'assertion-stream))

(define (get-stream key1 key2)
  (let ((s (get key1 key2)))
    (if s s the-empty-stream)))

(define THE-RULES the-empty-stream)

(define (fetch-rules pattern frame)
  (if (use-index? pattern)
      (get-indexed-rules pattern)
      (get-all-rules)))

(define (get-all-rules) THE-RULES)

(define (get-indexed-rules pattern)
  (stream-append
   (get-stream (index-key-of pattern) 'rule-stream)
   (get-stream '? 'rule-stream)))

(define (add-rule-or-assertion! assertion)
  (if (rule? assertion)
      (add-rule! assertion)
      (add-assertion! assertion)))

(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let ((old-assertions THE-ASSERTIONS))
    (set! THE-ASSERTIONS
          (cons-stream assertion old-assertions))
    'ok))

(define (add-rule! rule)
  (store-rule-in-index rule)
  (let ((old-rules THE-RULES))
    (set! THE-RULES (cons-stream rule old-rules))
    'ok))

(define (store-assertion-in-index assertion)
  (if (indexable? assertion)
      (let ((key (index-key-of assertion)))
        (let ((current-assertion-stream
               (get-stream key 'assertion-stream)))
          (put key
               'assertion-stream
               (cons-stream
                assertion
                current-assertion-stream))))))

(define (store-rule-in-index rule)
  (let ((pattern (conclusion rule)))
    (if (indexable? pattern)
        (let ((key (index-key-of pattern)))
          (let ((current-rule-stream
                 (get-stream key 'rule-stream)))
            (put key
                 'rule-stream
                 (cons-stream rule
                              current-rule-stream)))))))

(define (indexable? pat)
  (or (constant-symbol? (car pat))
      (var? (car pat))))

(define (use-index? pat) (constant-symbol? (car pat)))

(define (index-key-of pat)
  (let ((key (car pat)))
    (if (var? key) '? key)))

(define (assertion-to-be-added? exp)
  (eq? (type exp) 'assert!))
(define (add-assertion-body exp) (car (contents exp)))

(define (empty-conjunction? exps) (null? exps))
(define (first-conjunct exps) (car exps))
(define (rest-conjuncts exps) (cdr exps))
(define (empty-disjunction? exps) (null? exps))
(define (first-disjunct exps) (car exps))
(define (rest-disjuncts exps) (cdr exps))
(define (negated-query exps) (car exps))
(define (predicate exps) (car exps))
(define (args exps) (cdr exps))
(define (unique-query exps) (car exps))

(define (rule? statement)
  (tagged-list? statement 'rule))
(define (conclusion rule) (cadr rule))
(define (rule-body rule)
  (if (null? (cddr rule)) '(always-true) (caddr rule)))

(define (make-binding variable value)
  (cons variable value))
(define (binding-variable binding) (car binding))
(define (binding-value binding) (cdr binding))
(define (binding-in-frame variable frame)
  (assoc variable frame))
(define (extend variable value frame)
  (cons (make-binding variable value) frame))
(define (binding-in-frames var frames)
  (stream-filter (lambda (binding) (if binding #t #f)) (stream-map (lambda (frame) (binding-in-frame var frame)) frames)))

(define (query-syntax-process exp)
  (map-over-symbols expand-question-mark exp))

(define (map-over-symbols proc exp)
  (cond ((pair? exp)
         (cons (map-over-symbols proc (car exp))
               (map-over-symbols proc (cdr exp))))
        ((symbol? exp) (proc exp))
        (else exp)))

(define (expand-question-mark symbol)
  (let ((chars (symbol->string symbol)))
    (if (string=? (substring chars 0 1) "?")
        (list '?
              (string->symbol
               (substring chars 1 (string-length chars))))
        symbol)))

(define (var? exp) (tagged-list? exp '?))
(define (constant-symbol? exp) (symbol? exp))

(define (contract-question-mark variable)
  (string->symbol
   (string-append "?"
                  (if (number? (cadr variable))
                      (string-append (symbol->string (caddr variable))
                                     "-"
                                     (number->string (cadr variable)))
                      (symbol->string (cadr variable))))))

#|(define (install-and-qeval-pkg)
  (define (conjoin conjuncts frame-stream)
    (if (empty-conjunction? conjuncts)
        frame-stream
        (conjoin (rest-conjuncts conjuncts)
                 (qeval (first-conjunct conjuncts) frame-stream))))
  
  (put 'and 'qeval conjoin))|#

(define (install-and-qeval-pkg)
  (define (conjoin conjuncts frame-stream)
    (if (empty-conjunction? conjuncts)
        frame-stream
        (conjoin (rest-conjuncts conjuncts)
                 (qeval (first-conjunct conjuncts) frame-stream))))
  
  (put 'and 'qeval conjoin))

(define (merge-streams left right)
  (stream-flatmap
   (lambda (left-frame)
     (stream-filter
      succeeded?
      (stream-map
       (lambda (right-frame)
         (merge-frames left-frame right-frame))
       right)))
   left))
 
(define (succeeded? frame)
  (not (failed? frame)))
 
(define (failed? frame)
  (eq? 'failed frame))

(define empty-frame? null?)
(define first-binding car)
(define rest-bindings cdr)

(define (merge-frames left right)
  (cond ((or (failed? left)
             (failed? right))  'failed)
        ((empty-frame? left) right)
        (else (let* ((binding (first-binding left))
                     (var (binding-variable binding))
                     (val (binding-value binding))
                     (extension (extend-if-possible var val right)))
                (if (failed? extension)
                    'failed
                    (merge-frames (rest-bindings left) extension))))))

(define (install-or-qeval-pkg)
  (define (disjoin disjuncts frame-stream)
    (if (empty-disjunction? disjuncts)
        the-empty-stream
        (interleave-delayed
         (qeval (first-disjunct disjuncts) frame-stream)
         (delay (disjoin (rest-disjuncts disjuncts) frame-stream)))))
  (put 'or 'qeval disjoin))

(define (install-not-qeval-pkg)
  (define (negate operands frame-stream)
    (stream-flatmap
     (lambda (frame)
       (if (stream-null?
            (qeval (negated-query operands)
                   (singleton-stream frame)))
           (singleton-stream frame)
           the-empty-stream))
     frame-stream))
  (put 'not 'qeval negate))

(define (install-lisp-value-qeval-pkg)
  (define (lisp-value call frame-stream)
    (stream-flatmap
     (lambda (frame)
       (if (execute
            (instantiate
                call
              frame
              (lambda (v f)
                (error "Unknown pat var: LISP-VALUE" v))))
           (singleton-stream frame)
           the-empty-stream))
     frame-stream))
  (put 'lisp-value 'qeval lisp-value))

(define (install-unique-qeval-pkg)
  (define (uniquely-asserted operands frame-stream)
    (stream-flatmap
     (lambda (frame)
       (let ((extended-frame-stream (qeval (unique-query operands)
                                           (singleton-stream frame))))
         (cond ((stream-null? extended-frame-stream) the-empty-stream)
               ((stream-null? (stream-cdr extended-frame-stream)) extended-frame-stream)
               (else the-empty-stream))))
       frame-stream))
  (put 'unique 'qeval uniquely-asserted))

(define (execute exp)
  (apply (eval (predicate exp) user-initial-environment)
         (args exp)))

(define user-initial-environment '())

(define (install-always-true-qeval-pkg)
  (define (always-true ignore frame-stream) frame-stream)
  (put 'always-true 'qeval always-true))

(define (install-all)
  (install-and-qeval-pkg)
  (install-or-qeval-pkg)
  (install-not-qeval-pkg)
  (install-lisp-value-qeval-pkg)
  (install-always-true-qeval-pkg)
  (install-unique-qeval-pkg)
  'ok)
(install-all)

(define input-prompt ";;; Query input:")
(define output-prompt ";;; Query results:")
(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))
(define (announce-output string)
  (newline) (display string) (newline))

(define (instantiate exp frame unbound-var-handler)
  (define (copy exp)
    (cond ((var? exp)
           (let ((binding (binding-in-frame exp frame)))
             (if binding
                 (copy (binding-value binding))
                 (unbound-var-handler exp frame))))
          ((pair? exp)
           (cons (copy (car exp)) (copy (cdr exp))))
          (else exp)))
  (copy exp))
  
(define (query-driver-loop)
  (prompt-for-input input-prompt)
  (let ((q (query-syntax-process (read))))
    (cond ((assertion-to-be-added? q)
           (add-rule-or-assertion! (add-assertion-body q))
           (newline)
           (display "Assertion added to data base.")
           (query-driver-loop))
          (else
           (newline)
           (display output-prompt)
           (display-stream
            (stream-map
             (lambda (frame)
               (instantiate
                   q
                 frame
                 (lambda (v f)
                   (contract-question-mark v))))
             (qeval q (singleton-stream '()))))
           (reset-running-queries)
           (query-driver-loop)))))

(define database-assertions
  '((address (Warbucks Oliver) (Swellesley (Top Head Road)))
    (job (Warbucks Oliver) (administration big wheel))
    (salary (Warbucks Oliver) 150000)

    (address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))
    (job (Bitdiddle Ben) (computer wizard))
    (salary (Bitdiddle Ben) 60000)
    (supervisor (Bitdiddle Ben) (Warbucks Oliver))

    (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78))
    (job (Hacker Alyssa P) (computer programmer))
    (salary (Hacker Alyssa P) 40000)
    (supervisor (Hacker Alyssa P) (Bitdiddle Ben))

    (address (Fect Cy D) (Cambridge (Ames Street) 3))
    (job (Fect Cy D) (computer programmer))
    (salary (Fect Cy D) 35000)
    (supervisor (Fect Cy D) (Bitdiddle Ben))

    (address (Tweakit Lem E) (Boston (Bay State Road) 22))
    (job (Tweakit Lem E) (computer technician))
    (salary (Tweakit Lem E) 25000)
    (supervisor (Tweakit Lem E) (Bitdiddle Ben))

    (address (Reasoner Louis) (Slumerville (Pine Tree Road) 80))
    (job (Reasoner Louis) (computer programmer trainee))
    (salary (Reasoner Louis) 30000)
    (supervisor (Reasoner Louis) (Hacker Alyssa P))

    (address (Scrooge Eben) (Westen (Shady Lane) 10))
    (job (Scrooge Eben) (accounting chief accountant))
    (salary (Scrooge Eben) 75000)
    (supervisor (Scrooge Eben) (Warbucks Oliver))

    (address (Cratchet Robert) (Allston (N Harvard Street) 16))
    (job (Cratchet Robert) (accounting scrivener))
    (salary (Cratchet Robert) 18000)
    (supervisor (Cratchet Robert) (Scrooge Eben))

    (address (Aull DeWitt) (Slumerville (Onion Square) 5))
    (job (Aull DeWitt) (administration secretary))
    (salary (Aull DeWitt) 25000)
    (supervisor (Aull DeWitt) (Warbucks Oliver))

    (meeting accounting (Monday 9am))
    (meeting administration (Monday 10am))
    (meeting computer (Wednesday 3pm))
    (meeting administration (Friday 1pm))
    (meeting whole-company (Wednesday 4pm))

    (can-do-job (computer wizard) (computer programmer))
    (can-do-job (computer wizard) (computer technician))
    (can-do-job (computer programmer) (computer programmer trainee))
    (can-do-job (administration secretary) (administration big wheel))
    
    (son Adam Cain)
    (son Cain Enoch)
    (son Enoch Irad)
    (son Irad Mehujael)
    (son Mehujael Methushael)
    (son Methushael Lamech)
    (wife Lamech Ada)
    (son Ada Jabal)
    ((great grandson) Adam Irad)

    (married Minnie Mickey)
    (rule (married ?x ?y) (married ?y ?x))

    (rule (lives-near ?person-1 ?person-2)
          (and (address ?person-1 (?town . ?rest-1))
               (address ?person-2 (?town . ?rest-2))
               (not (same ?person-1 ?person-2))))
    
    (rule (can-replace ?person-1 ?person-2)
          (and (job ?person-1 ?job-1)
               (job ?person-2 ?job-2)
               (not (same ?person-1 ?person-2))
               (or (same ?job-1 ?job-2)
                   (can-do-job ?job-1 ?job-2))))
    
    (rule (same ?x ?x))

    (rule (wheel ?person)
          (and (supervisor ?middle-manager ?person)
               (supervisor ?x ?middle-manager)))

    #|(rule (outranked-by ?staff-person ?boss)
          (or (supervisor ?staff-person ?boss)
              (and (supervisor ?staff-person ?middle-manager)
                   (outranked-by ?middle-manager ?boss))))|#
    
    (rule (outranked-by ?staff-person ?boss)
          (or (supervisor ?staff-person ?boss)
              (and (outranked-by ?middle-manager ?boss)
                   (supervisor ?staff-person
                               ?middle-manager))))

    (rule (big-shot ?person ?division)
          (and (job ?person (?division . ?title))
               (or (not (supervisor ?person ?sprvisr))
                   (and (supervisor ?person ?sprvisr)
                        (not (job ?sprvisr (?division . ?title2)))))))

    (rule (meeting-time ?person ?date-time)
          (or (and (job ?person (?department . ?rest))
                   (meeting ?department ?date-time))
              (meeting whole-company ?date-time)))

    (rule (grandson ?g ?s)
          (and (son ?f ?s)
               (son ?g ?f)))
    
    (rule (ends-in-grandson (grandson)))
    (rule (ends-in-grandson (?a . ?b))
          (ends-in-grandson ?b))
    
    (rule ((great . ?rel) ?x ?y)
      (and (ends-in-grandson ?rel)
           (son ?x ?other)
           (?rel ?other ?y)))
    
    #|(rule ((great . ?rel) ?x ?y)
          (and (?rel (grandson))
               (son ?f ?y)
               (son ?gf ?f)
               (son ?x ?gf)))
    
    (rule ((great . ?rel) ?x ?y)
          (and (ends-in-grandson ?rel)
               (not (?rel (grandson)))
               (?rel ?x ?y)))|#
    
    (rule (son ?f ?s)
          (and (?m is wife of ?f)
               (son ?m ?s)))
          
    (rule (?x next-to ?y in (?x ?y . ?u)))
    (rule (?x next-to ?y in (?v . ?z))
          (?x next-to ?y in ?z))

    #|(rule (last-pair ?list ?last)
          (and (same ?last (?a))
               (or (and (same ?list (?first . ?rest))
                        (not (same ?rest ()))
                        (or (same ?last ?rest)
                            (last-pair ?rest ?last)))
                   (same ?last ?list))))|#
    
    (rule (last-pair (?a) (?a)))
    (rule (last-pair (?x . ?y) ?last)
          (last-pair ?y ?last))

    (rule (reverse () ()))
    (rule (reverse (?x) (?x)))
    (rule (reverse (?x . ?y) ?rev)
          (and (reverse ?y ?z)
               (append-to-form ?z (?x) ?rev)))
    (rule (reverse ?rev (?x . ?y))
          (and (reverse ?y ?z)
               (append-to-form ?z (?x) ?rev))) 
   
    (rule (append-to-form () ?y ?y))
    (rule (append-to-form (?u . ?v) ?y (?u . ?z))
          (append-to-form ?v ?y ?z))))

(define (add-to-data-base! assertions)
  (for-each add-rule-or-assertion! (query-syntax-process assertions)))

(add-to-data-base! database-assertions)

(query-driver-loop)