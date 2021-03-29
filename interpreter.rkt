#lang racket
(require "simpleParser.rkt")

; Group 4: Aranya Kumar, Kris Tran, Kyle Ngo

; 3 errors
(define breakOutsideLoopError
  (lambda (env) (error 'Break-used-outside-loop)))

(define continueOutsideLoopError
  (lambda (env) (error 'Continue-used-outside-of-loop)))

(define uncaughtExceptionThrownError
  (lambda (v env) (error 'Uncaught-exception-thrown)))

;Interpret abstraction: initializes the state to be null with no variables
(define initialstate '((()())))

; interpret: gets the a parsed list of code from a file
(define interpret
  (lambda (filename)
    (call/cc
      (lambda (return)
        (interpret-code-block (parser filename) initialstate return
                                  breakOutsideLoopError continueOutsideLoopError
                                  uncaughtExceptionThrownError)))))

(define interpret-code-block
  (lambda (code-block-list state return break continue throw)
    (if (null? code-block-list)
        state
        (interpret-code-block (rest-of-code code-block-list) (M-state (first-code-block code-block-list) state return break continue throw) return break continue throw))))
    
(define rest-of-code cdr)
(define first-code-block car)

; M-state: goes through parsed code and calls applicable functions to run the code
(define M-state
  (lambda (stmt state return break continue throw)
    (cond
      ((eq? (operator stmt) 'var) (M-state-declaration stmt state))
      ((eq? (operator stmt) '=) (M-state-assignment stmt state))
      ((eq? (operator stmt) 'if) (M-state-if stmt state return break continue throw))
      ((eq? (operator stmt) 'while) (M-state-while stmt state return throw))
      ((eq? (operator stmt) 'return) (M-state-return stmt state return throw))
      ((eq? (operator stmt) 'throw) (M-state-throw stmt state throw))
      ((eq? (operator stmt) 'continue) (continue state))
      ((eq? (operator stmt) 'break) (break state))
      ((eq? (operator stmt) 'begin) (M-state-begin stmt state return break continue throw)) 
      ;((eq? (operator stmt) 'try) (M-state-try stmt state return break continue throw))  ; Need to implement
      (else (error 'statement-not-defined)))))

; M-integer: computes the value of an expression. Outputs a value or bad operator.
(define M-integer
  (lambda (expression state)
    (cond
      ((number? expression) expression)
      ((name? expression) (lookupbinding expression state)) 
      ((eq? (operator expression) '+) (+ (M-integer (leftoperand expression) state) (M-integer (rightoperand expression) state)))
      ((eq? (operator expression) '-) (if (null? (cddr expression)) (- 0 (M-integer (leftoperand expression) state))
                                (- (M-integer (leftoperand expression) state) (M-integer (rightoperand expression) state))))
      ((eq? (operator expression) '/) (quotient (M-integer (leftoperand expression) state) (M-integer (rightoperand expression) state)))
      ((eq? (operator expression) '%) (remainder (M-integer (leftoperand expression) state) (M-integer (rightoperand expression) state)))
      ((eq? (operator expression) '*) (* (M-integer (leftoperand expression) state) (M-integer (rightoperand expression) state)))
      (else (error 'bad-operator)))))

; M-boolean: compares right and left operands of a boolean expression. Outputs true, false, or bad operator.
(define M-boolean
  (lambda (expression state)
    (cond
      ((eq? (operator expression) '#t) #t)
      ((eq? (operator expression) '#f) #f)
      ((eq? (operator expression) '<) (< (M-integer (leftoperand expression) state) (M-integer (rightoperand expression) state)))
      ((eq? (operator expression) '<=) (<= (M-integer (leftoperand expression) state) (M-integer (rightoperand expression) state)))
      ((eq? (operator expression) '>) (> (M-integer (leftoperand expression) state) (M-integer (rightoperand expression) state)))
      ((eq? (operator expression) '>=) (>= (M-integer (leftoperand expression) state) (M-integer (rightoperand expression) state)))
      ((eq? (operator expression) '==) (eq? (M-integer (leftoperand expression) state) (M-integer (rightoperand expression) state)))
      ((eq? (operator expression) '!=) (not (eq? (M-integer (leftoperand expression) state) (M-integer (rightoperand expression) state))))
      ((eq? (operator expression) '||) (or (M-boolean (leftoperand expression) state) (M-boolean (rightoperand expression) state)))
      ((eq? (operator expression) '&&) (and (M-boolean (leftoperand expression) state) (M-boolean (rightoperand expression) state)))
      ((eq? (operator expression) '!) (not (M-boolean (leftoperand expression) state)))
      (else (error 'bad-operator)))))

; Operators & Operands Abstractions
;(define operator (lambda (expression) (car expression)))
(define leftoperand cadr)
(define rightoperand (lambda (expression) (caddr expression)))

; Addbinding: stores value in a variable
(define addbinding
  (lambda (name value state)
    (if (null? (cdr state))
        (list (list (add-to-end name (car (car state))) (add-to-end value (car (cdr (car state))))))
        ; new logic appending into the left-most frame
        (cons (list (add-to-end name (car (car state))) (add-to-end value (car (cdr (car state))))) (remove-frame state)))))
        
; Removebinding: removes the value from a variable
(define removebinding
  (lambda (name state)
    (cond
      ((null? state) state)
      (else (list (removefirst name (variables state)) (remove-element-by-index (index name (variables state)) (values state)))))))

; Lookupbinding: looks up the value of a given variable
(define lookupbinding
  (lambda (name state)
    (if (null? (cdr state))
        (element-at-index (index name (car (car state))) (car (cdr (car state))))
        ; go thru right-most to left-most layer, to get the value
        (if (equal? (element-at-index (index name (car (car state))) (car (cdr (car state)))) 'not-here)
            ; keep going to next layer
            (lookupbinding name (cdr state))
            ; else, return the value
            (element-at-index (index name (car (car state))) (car (cdr (car state))))))))

; Abstrations for bindings
(define variables car)
(define values cadr)
      
; M-state-assignment: computes expression and registers value of expression to variable name
(define M-state-assignment
  (lambda (expression state)
    (addbinding (variable expression) (M-integer (expression-value expression) state) (removebinding (variable expression) state))))

; M-state-declaration: 
(define M-state-declaration
  (lambda (expression state)
    (cond
      ((null? (cddr expression)) (addbinding (variable expression) 'null state))
      ((declared? (variable expression) (car (car state))) (addbinding (variable expression) (M-integer (expression-value expression) state) (removebinding (variable expression) state)))
      (else (addbinding (variable expression) (M-integer (expression-value expression) state) state)))))


; Abstractions for M-state-assignment and declaration
(define variable cadr)
(define expression-value (lambda (expression) (caddr expression)))

; M-state-while with break and continue implemented
(define M-state-while
  (lambda (stmt state return throw)
    (call/cc
     (lambda (break)
       (letrec ((loop (lambda (condition body state)
                        (if (M-boolean condition state)
                            (loop condition body (M-state body state return break (lambda (v) (break (loop condition body v))) throw)) state))))
         (loop (condition stmt) (body stmt) state))))))

; Abstraction for M-state-while
(define condition cadr)
(define body caddr)

; M-state-return: returns the value of the expression or variable
(define M-state-return
  (lambda (returnstate state return throw)
    (cond
      ((pair? (cadr returnstate))
       (if (isBool (cadr returnstate))
           (M-boolean (cadr returnstate) state)
           (M-integer (cadr returnstate) state)))
      ((eq? (cadr returnstate) '#t) #t)
      ((eq? (cadr returnstate) '#f) #f)
      (else (M-integer (cadr returnstate) state)))))


; M-state-if: if the condition is true, it executes statement1, if not, executes statement2
(define M-state-if
  (lambda (ifstate state return break continue throw)
    (cond
      ((M-boolean (ifcondition ifstate) state)
       (M-state (ifbody ifstate) state return break continue throw))
      ((not (null? (cdddr ifstate))) (M-state (nextif ifstate) state return break continue throw))
      (else state))))

; Abstraction for M-state-if
(define ifcondition (lambda (ifstate) (cadr ifstate)))
(define ifbody (lambda (ifstate) (caddr ifstate)))
(define nextif (lambda (ifstate) (cadddr ifstate)))



(define M-state-throw
  (lambda (throwblock state throw)
    (throw (M-integer (expression-of throwblock) state throw) state)))

; handle code block
(define M-state-begin
  (lambda (statement state return break continue throw)
    (remove-frame (interpret-code-block (cdr statement)
                                         (add-frame state)
                                         return
                                         (lambda (s) (break (remove-frame s)))
                                         (lambda (s) (continue (remove-frame s)))
                                         (lambda (v s) (throw v (remove-frame s)))))))

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
(define expression-of operand1)
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

; add a new frame on the top of the state
(define add-frame
  (lambda (state)
    (cons '(() ()) state)))

; remove a frame from the state
(define remove-frame
  (lambda (state)
    (cdr state)))

; return true if x is a name of a variable (not a list or a number)
(define name?
  (lambda (x)
    (and (not (list? x)) (not (number? x)))))
; add an element to the end of the list
(define add-to-end
  (lambda (a lis)
    (cond
      ((null? lis) (cons a lis))
      (else (cons (car lis) (add-to-end a (cdr lis)))))))
; remove the first occurence of an element from the list
(define removefirst
  (lambda (a lis)
    (cond
      ((null? lis) lis)
      ((eq? a (car lis)) (cdr lis))
      (else (cons (car lis) (removefirst a (cdr lis)))))))
; return the index of an element in the list
(define index
  (lambda (a lis)
    (cond
      ((null? lis) -1)
      ((equal? (car lis) a) 0)
      (else (+ 1 (index a (cdr lis)))))))

; remove an element at a given index
(define remove-element-by-index
  (lambda (a lis)
    (cond
      ((null? lis) lis)
      ((= a 0) (cdr lis))
      (else (cons (car lis) (remove-element-by-index (- a 1) (cdr lis)))))))

; return an element at a given index
(define element-at-index
  (lambda (a lis)
    (cond
      ((= a -1) 'not-here)
      ((= a 0)
       (if (eq? (car lis) 'null)
           (error 'unassigned-variable)
           (car lis)))
      (else (element-at-index (- a 1) (cdr lis))))))

; return if an expression is a boolean expression
(define isBool
  (lambda (expression)
    (cond
      ((or (eq? (car expression) '||) (eq? (car expression) '&&)) #t)
      ((or (eq? (car expression) '<) (eq? (car expression) '<=)) #t)
      ((or (eq? (car expression) '>) (eq? (car expression) '>=)) #t)
      ((or (eq? (car expression) '==) (eq? (car expression) '!=)) #t)
      ((eq? (car expression) '!) #t)
      (else #f))))

; check if a variable has been declared
(define declared?
  (lambda (x declared)
    (cond
      ((null? declared) #f)
      ((eq? x (car declared)) #t)
      (else (declared? x (cdr declared))))))
