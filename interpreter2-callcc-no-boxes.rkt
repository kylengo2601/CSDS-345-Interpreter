; If you are using scheme instead of racket, comment these two lines, uncomment the (load "simpleParser.scm") and comment the (require "simpleParser.rkt")
#lang racket
(require "functionParser.rkt")
; (load "simpleParser.scm")


; An interpreter for the simple language that uses call/cc for the continuations.  Does not handle side effects.
(define call/cc call-with-current-continuation)


; The functions that start interpret-...  all return the current environment.
; The functions that start eval-...  all return a value

; The main function.  Calls parser to get the parse tree and interprets it with a new environment.  The returned value is in the environment.
(define interpret
  (lambda (file)
    (scheme->language
     (call/cc
      (lambda (return)
        (interpret-statement-list (parser file) (newenvironment) return
                                  (lambda (env) (myerror "Break used outside of loop")) (lambda (env) (myerror "Continue used outside of loop"))
                                  (lambda (v env) (myerror "Uncaught exception thrown"))))))))

; interprets a list of statements.  The environment from each statement is used for the next ones.
; Mstate (<statement><statement-list>, state) = Mstate(<statement-list>, Mstate(<statement>, state))

(define interpret-statement-list
  (lambda (statement-list environment return break continue throw)
    (if (null? statement-list)
        environment
        (interpret-statement-list (cdr statement-list) (interpret-statement (car statement-list) environment return break continue throw) return break continue throw))))

; interpret a statement in the environment with continuations for return, break, continue, throw
(define interpret-statement
  (lambda (statement environment return break continue throw)
    (cond
      ((eq? 'return (statement-type statement)) (interpret-return statement environment return))
      ((eq? 'var (statement-type statement)) (interpret-declare statement environment))
      ((eq? '= (statement-type statement)) (interpret-assign statement environment))
      ((eq? 'if (statement-type statement)) (interpret-if statement environment return break continue throw))
      ((eq? 'while (statement-type statement)) (interpret-while statement environment return throw))
      ((eq? 'continue (statement-type statement)) (continue environment))
      ((eq? 'break (statement-type statement)) (break environment))
      ((eq? 'begin (statement-type statement)) (interpret-block statement environment return break continue throw))
      ((eq? 'throw (statement-type statement)) (interpret-throw statement environment throw))
      ((eq? 'try (statement-type statement)) (interpret-try statement environment return break continue throw))
      ((eq? 'function (statement-type statement)) (interpret-function-binding (cdr statement) environment return break continue throw))
      ((eq? 'funcall (statement-type statement)) (interpret-funcall-result-environment (statement-list-from-function (lookup (function-name (statement-without-funcall statement)) environment)) (add-parameters-to-environment (get-parameters (lookup (function-name (statement-without-funcall statement)) environment)) (parameters (statement-without-funcall statement)) (push-frame environment) throw)
                                                                                       return
                                                                                       break continue throw))
      (else (myerror "Unknown statement:" (statement-type statement))))))

(define statement-list-from-function cadr)
(define statement-without-func cdr)
(define statement-without-funcall statement-without-func)
(define function-name car)
(define get-parameters car)
(define parameters cdr)

; Calls the return continuation with the given expression value
(define interpret-return
  (lambda (statement environment return)
    (return (eval-expression (get-expr statement) environment))))

; Return environment resulted from calling function
(define interpret-funcall-result-environment
 (lambda (statement-list environment return break continue throw)
  (cond
    ((null? statement-list) environment)
    (else (if (list? (call/cc
                      (lambda (breakreturn)
                        (interpret-statement (first-statement statement-list) environment breakreturn break continue throw))))
                  (interpret-funcall-result-environment (rest-of-statement-list statement-list) (call/cc
                                                                              (lambda (breakreturn)
                                                                                (interpret-statement (first-statement statement-list) environment breakreturn break continue throw)))
                                                        return break continue throw)
                  environment)))))

(define first-statement car)
(define rest-of-statement-list cdr)

; Adds a new variable binding to the environment.  There may be an assignment with the variable
(define interpret-declare
  (lambda (statement environment)
    (if (exists-declare-value? statement)
        (insert (get-declare-var statement) (eval-expression (get-declare-value statement) environment) environment)
        (insert (get-declare-var statement) 'novalue environment))))

; Updates the environment to add an new binding for a variable
(define interpret-assign
  (lambda (statement environment)
    (update (get-assign-lhs statement) (eval-expression (get-assign-rhs statement) environment) environment)))

; We need to check if there is an else condition.  Otherwise, we evaluate the expression and do the right thing.
(define interpret-if
  (lambda (statement environment return break continue throw)
    (cond
      ((eval-expression (get-condition statement) environment) (interpret-statement (get-then statement) environment return break continue throw))
      ((exists-else? statement) (interpret-statement (get-else statement) environment return break continue throw))
      (else environment))))

; Interprets a while loop.  We must create break and continue continuations for this loop
(define interpret-while
  (lambda (statement environment return throw)
    (call/cc
     (lambda (break)
       (letrec ((loop (lambda (condition body environment)
                        (if (eval-expression condition environment)
                            (loop condition body (interpret-statement body environment return break (lambda (env) (break (loop condition body env))) throw))
                         environment))))
         (loop (get-condition statement) (get-body statement) environment))))))

; Interprets a block.  The break, continue, and throw continuations must be adjusted to pop the environment
; Mstate({ <body> }, state) = pop-frame (Mstate (<body>, pushframe(state)))
(define interpret-block
  (lambda (statement environment return break continue throw)
    (pop-frame (interpret-statement-list (cdr statement)
                                         (push-frame environment)
                                         return
                                         (lambda (env) (break (pop-frame env)))
                                         (lambda (env) (continue (pop-frame env)))
                                         (lambda (v env) (throw v (pop-frame env)))))))

; We use a continuation to throw the proper value. Because we are not using boxes, the environment/state must be thrown as well so any environment changes will be kept
(define interpret-throw
  (lambda (statement environment throw)
    (throw (eval-expression (get-expr statement) environment) environment)))

; Interpret a try-catch-finally block

; Create a continuation for the throw.  If there is no catch, it has to interpret the finally block, and once that completes throw the exception.
;   Otherwise, it interprets the catch block with the exception bound to the thrown value and interprets the finally block when the catch is done
(define create-throw-catch-continuation
  (lambda (catch-statement environment return break continue throw jump finally-block)
    (cond
      ((null? catch-statement) (lambda (ex env) (throw ex (interpret-block finally-block env return break continue throw)))) 
      ((not (eq? 'catch (statement-type catch-statement))) (myerror "Incorrect catch statement"))
      (else (lambda (ex env)
              (jump (interpret-block finally-block
                                     (pop-frame (interpret-statement-list 
                                                 (get-body catch-statement) 
                                                 (insert (catch-var catch-statement) ex (push-frame env))
                                                 return 
                                                 (lambda (env2) (break (pop-frame env2))) 
                                                 (lambda (env2) (continue (pop-frame env2))) 
                                                 (lambda (v env2) (throw v (pop-frame env2)))))
                                     return break continue throw)))))))

; To interpret a try block, we must adjust  the return, break, continue continuations to interpret the finally block if any of them are used.
;  We must create a new throw continuation and then interpret the try block with the new continuations followed by the finally block with the old continuations
(define interpret-try
  (lambda (statement environment return break continue throw)
    (call/cc
     (lambda (jump)
       (let* ((finally-block (make-finally-block (get-finally statement)))
              (try-block (make-try-block (get-try statement)))
              (new-return (lambda (v) (begin (interpret-block finally-block environment return break continue throw) (return v))))
              (new-break (lambda (env) (break (interpret-block finally-block env return break continue throw))))
              (new-continue (lambda (env) (continue (interpret-block finally-block env return break continue throw))))
              (new-throw (create-throw-catch-continuation (get-catch statement) environment return break continue throw jump finally-block)))
         (interpret-block finally-block
                          (interpret-block try-block environment new-return new-break new-continue new-throw)
                          return break continue throw))))))

; helper methods so that I can reuse the interpret-block method on the try and finally blocks
(define make-try-block
  (lambda (try-statement)
    (cons 'begin try-statement)))

(define make-finally-block
  (lambda (finally-statement)
    (cond
      ((null? finally-statement) '(begin))
      ((not (eq? (statement-type finally-statement) 'finally)) (myerror "Incorrectly formatted finally block"))
      (else (cons 'begin (cadr finally-statement))))))

; Evaluates all possible boolean and arithmetic expressions, including constants and variables.
; Add function call handling
(define eval-expression
  (lambda (expr environment)
    (cond
      ((number? expr) expr)
      ((eq? expr 'true) #t)
      ((eq? expr 'false) #f)
      ((not (list? expr)) (lookup expr environment))
      ((eq? (operator expr) 'funcall) (function-execution (cdr expr) environment))
      (else (eval-operator expr environment)))))

; Evaluate a binary (or unary) operator.  Although this is not dealing with side effects, I have the routine evaluate the left operand first and then
; pass the result to eval-binary-op2 to evaluate the right operand.  This forces the operands to be evaluated in the proper order in case you choose
; to add side effects to the interpreter
(define eval-operator
  (lambda (expr environment)
    (cond
      ((eq? '! (operator expr)) (not (eval-expression (operand1 expr) environment)))
      ((and (eq? '- (operator expr)) (= 2 (length expr))) (- (eval-expression (operand1 expr) environment)))
      (else (eval-binary-op2 expr (eval-expression (operand1 expr) environment) environment)))))

; Complete the evaluation of the binary operator by evaluating the second operand and performing the operation.
(define eval-binary-op2
  (lambda (expr op1value environment)
    (cond
      ((eq? '+ (operator expr)) (+ op1value (eval-expression (operand2 expr) environment)))
      ((eq? '- (operator expr)) (- op1value (eval-expression (operand2 expr) environment)))
      ((eq? '* (operator expr)) (* op1value (eval-expression (operand2 expr) environment)))
      ((eq? '/ (operator expr)) (quotient op1value (eval-expression (operand2 expr) environment)))
      ((eq? '% (operator expr)) (remainder op1value (eval-expression (operand2 expr) environment)))
      ((eq? '== (operator expr)) (isequal op1value (eval-expression (operand2 expr) environment)))
      ((eq? '!= (operator expr)) (not (isequal op1value (eval-expression (operand2 expr) environment))))
      ((eq? '< (operator expr)) (< op1value (eval-expression (operand2 expr) environment)))
      ((eq? '> (operator expr)) (> op1value (eval-expression (operand2 expr) environment)))
      ((eq? '<= (operator expr)) (<= op1value (eval-expression (operand2 expr) environment)))
      ((eq? '>= (operator expr)) (>= op1value (eval-expression (operand2 expr) environment)))
      ((eq? '|| (operator expr)) (or op1value (eval-expression (operand2 expr) environment)))
      ((eq? '&& (operator expr)) (and op1value (eval-expression (operand2 expr) environment)))
      (else (myerror "Unknown operator:" (operator expr))))))

; Binds a function name to its closure.
(define interpret-function-binding
  (lambda (statement environment return break continue throw)
    (cond
      ((null? (func-body statement)) environment) ;checks if the function body is empty
      ((eq? (func-name statement) 'main) (interpret-statement (main-func-body statement) environment return break continue throw))
      (else (insert (func-name statement) (func-body statement) environment)))))

(define func-name car)
(define func-body cdr)
(define main-func-body caaddr)
(define statement-list-of-function cadr)

; Iterates through the parameters of a function and adds them to the frame of environment.
(define iterate-params
  (lambda (names environment)
    (cond
      ((null? names) environment)
    (insert (cdr names) (insert (car names) 'null environment)))))

; Calls the function closure when the name is called.
(define interpret-function-call
  (lambda (statement environment return break continue throw)
    (interpret-block (lookup-in-env (car (cdr (car statement))) environment))))

; adds the given parameters to the givene environment
(define add-parameters-to-environment
  (lambda (param-names param-values environment throw)
    (cond
      ((null? param-names) environment)
      ((not (eq? (length param-names) (length param-values))) (myerror "Mismatching parameters and arguments"))
      ((list? param-names) (add-parameters-to-environment (parameters param-names) (parameters param-values) (insert (first param-names) (eval-expression (first param-values) (pop-frame environment) throw) environment) throw))
      (else (insert param-names (eval-expression param-values (pop-frame environment)) environment)))))

(define function-execution
  (lambda (funcall envr)
    (call/cc
     (lambda (return)
       (let* ((closure (lookup-in-env (car funcall)))
              (body (car closure))
              (formal-params (cadr closure))
              (actual-params (cdr funcall))
              (fstate1 ((caddr closure) envr))
              (fstate2 (interpret-function-binding formal-params actual-params envr (push-frame fstate1))))
         (interpret-statement body fstate2 (lambda (env) (myerror "Break used outside of loop")) return (lambda (env) (myerror "Continue used outside of loop")) return))))))

; Determines if two values are equal.  We need a special test because there are both boolean and integer types.
(define isequal
  (lambda (val1 val2)
    (if (and (number? val1) (number? val2))
        (= val1 val2)
        (eq? val1 val2))))

;-----------------
; HELPER FUNCTIONS
;-----------------

; These helper functions define the operator and operands of a value expression
(define operator car)
(define operand1 cadr)
(define operand2 caddr)
(define operand3 cadddr)

(define exists-operand2?
  (lambda (statement)
    (not (null? (cddr statement)))))

(define exists-operand3?
  (lambda (statement)
    (not (null? (cdddr statement)))))

; these helper functions define the parts of the various statement types
(define statement-type operator)
(define get-expr operand1)
(define get-declare-var operand1)
(define get-declare-value operand2)
(define exists-declare-value? exists-operand2?)
(define get-assign-lhs operand1)
(define get-assign-rhs operand2)
(define get-condition operand1)
(define get-then operand2)
(define get-else operand3)
(define get-body operand2)
(define exists-else? exists-operand3?)
(define get-try operand1)
(define get-catch operand2)
(define get-finally operand3)

(define catch-var
  (lambda (catch-statement)
    (car (operand1 catch-statement))))


;------------------------
; Environment/State Functions
;------------------------

; create a new empty environment
(define newenvironment
  (lambda ()
    (list (newframe))))

; create an empty frame: a frame is two lists, the first are the variables and the second is the "store" of values
(define newframe
  (lambda ()
    '(() ())))

; add a frame onto the top of the environment
(define push-frame
  (lambda (environment)
    (cons (newframe) environment)))

; remove a frame from the environment
(define pop-frame
  (lambda (environment)
    (cdr environment)))

; some abstractions
(define topframe car)
(define remainingframes cdr)

; does a variable exist in the environment?
(define exists?
  (lambda (var environment)
    (cond
      ((null? environment) #f)
      ((exists-in-list? var (variables (topframe environment))) #t)
      (else (exists? var (remainingframes environment))))))

; does a variable exist in a list?
(define exists-in-list?
  (lambda (var l)
    (cond
      ((null? l) #f)
      ((eq? var (car l)) #t)
      (else (exists-in-list? var (cdr l))))))

; Looks up a value in the environment.  If the value is a boolean, it converts our languages boolean type to a Scheme boolean type
(define lookup
  (lambda (var environment)
    (lookup-variable var environment)))
  
; A helper function that does the lookup.  Returns an error if the variable does not have a legal value
(define lookup-variable
  (lambda (var environment)
    (let ((value (lookup-in-env var environment)))
      (if (eq? 'novalue value)
          (myerror "error: variable without an assigned value:" var)
          value))))

; Return the value bound to a variable in the environment
(define lookup-in-env
  (lambda (var environment)
    (cond
      ((null? environment) (myerror "error: undefined variable" var))
      ((exists-in-list? var (variables (topframe environment))) (lookup-in-frame var (topframe environment)))
      (else (lookup-in-env var (cdr environment))))))

; Return the value bound to a variable in the frame
(define lookup-in-frame
  (lambda (var frame)
    (cond
      ((not (exists-in-list? var (variables frame))) (myerror "error: undefined variable" var))
      (else (language->scheme (get-value (indexof var (variables frame)) (store frame)))))))

; Get the location of a name in a list of names
(define indexof
  (lambda (var l)
    (cond
      ((null? l) 0)  ; should not happen
      ((eq? var (car l)) 0)
      (else (+ 1 (indexof var (cdr l)))))))

; Get the value stored at a given index in the list
(define get-value
  (lambda (n l)
    (cond
      ((zero? n) (car l))
      (else (get-value (- n 1) (cdr l))))))

; Adds a new variable/value binding pair into the environment.  Gives an error if the variable already exists in this frame.
(define insert
  (lambda (var val environment)
    (if (exists-in-list? var (variables (car environment)))
        (myerror "error: variable is being re-declared:" var)
        (cons (add-to-frame var val (car environment)) (cdr environment)))))

; Changes the binding of a variable to a new value in the environment.  Gives an error if the variable does not exist.
(define update
  (lambda (var val environment)
    (if (exists? var environment)
        (update-existing var val environment)
        (myerror "error: variable used but not defined:" var))))

; Add a new variable/value pair to the frame.
(define add-to-frame
  (lambda (var val frame)
    (list (cons var (variables frame)) (cons (scheme->language val) (store frame)))))

; Changes the binding of a variable in the environment to a new value
(define update-existing
  (lambda (var val environment)
    (if (exists-in-list? var (variables (car environment)))
        (cons (update-in-frame var val (topframe environment)) (remainingframes environment))
        (cons (topframe environment) (update-existing var val (remainingframes environment))))))

; Changes the binding of a variable in the frame to a new value.
(define update-in-frame
  (lambda (var val frame)
    (list (variables frame) (update-in-frame-store var val (variables frame) (store frame)))))

; Changes a variable binding by placing the new value in the appropriate place in the store
(define update-in-frame-store
  (lambda (var val varlist vallist)
    (cond
      ((eq? var (car varlist)) (cons (scheme->language val) (cdr vallist)))
      (else (cons (car vallist) (update-in-frame-store var val (cdr varlist) (cdr vallist)))))))

; Returns the list of variables from a frame
(define variables
  (lambda (frame)
    (car frame)))

; Returns the store from a frame
(define store
  (lambda (frame)
    (cadr frame)))

; Functions to convert the Scheme #t and #f to our languages true and false, and back.

(define language->scheme
  (lambda (v) 
    (cond 
      ((eq? v 'false) #f)
      ((eq? v 'true) #t)
      (else v))))

(define scheme->language
  (lambda (v)
    (cond
      ((eq? v #f) 'false)
      ((eq? v #t) 'true)
      (else v))))



; Because the error function is not defined in R5RS scheme, I create my own:
(define error-break (lambda (v) v))
(call-with-current-continuation (lambda (k) (set! error-break k)))

(define myerror
  (lambda (str . vals)
    (letrec ((makestr (lambda (str vals)
                        (if (null? vals)
                            str
                            (makestr (string-append str (string-append " " (symbol->string (car vals)))) (cdr vals))))))
      (error-break (display (string-append str (makestr "" vals)))))))
