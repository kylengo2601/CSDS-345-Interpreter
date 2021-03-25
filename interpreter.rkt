#lang racket
(require "simpleParser.rkt")

; Group 4: Aranya Kumar, Kris Tran, Kyle Ngo

(define interpret
  (lambda (filename)
    (M-state (parser filename) '(()()))))

; M-integer
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

; M-boolean
(define M-boolean
  (lambda (expression state)
    (cond
      ((eq? (operator expression) '#t) #t)
      ((eq? (operator expression) '#f) #f)
      ((eq? (operator expression) '<) (< (M-integer (leftoperand expression) state) (M-integer (rightoperand expression) state)))
      ((eq? (operator expression) '<=) (or (< (M-integer (leftoperand expression) state) (M-integer (rightoperand expression) state)) (eq? (M-integer (leftoperand expression) state) (M-integer (rightoperand expression) state))))
      ((eq? (operator expression) '>) (> (M-integer (leftoperand expression) state) (M-integer (rightoperand expression) state)))
      ((eq? (operator expression) '>=) (or (> (M-integer (leftoperand expression) state) (M-integer (rightoperand expression) state)) (eq? (M-integer (leftoperand expression) state) (M-integer (rightoperand expression) state))))
      ((eq? (operator expression) '==) (eq? (M-integer (leftoperand expression) state) (M-integer (rightoperand expression) state)))
      ((eq? (operator expression) '!=) (not (eq? (M-integer (leftoperand expression) state) (M-integer (rightoperand expression) state))))
      ((eq? (operator expression) '||) (or (M-boolean (leftoperand expression) state) (M-boolean (rightoperand expression) state)))
      ((eq? (operator expression) '&&) (and (M-boolean (leftoperand expression) state) (M-boolean (rightoperand expression) state)))
      ((eq? (operator expression) '!) (not (M-boolean (leftoperand expression) state)))
      (else (error 'bad-operator)))))

; Operators & Operands Abstractions
(define operator (lambda (expression) (car expression)))
(define leftoperand cadr)
(define rightoperand (lambda (expression) (caddr expression)))

(define addbinding
  (lambda (name value state) (list (add-to-end name (car state)) (add-to-end value (cadr state)))))

(define removebinding
  (lambda (name state)
    (cond
      ((null? state) state)
      (else (list (removefirst name (car state)) (remove-element-by-index (index name (car state)) (cadr state)))))))
 
(define lookupbinding
  (lambda (name state) (element-at-index (index name (car state)) (cadr state))))
      
; M-state-assignment
(define M-state-assignment
  (lambda (expression state)
    (addbinding (variable expression) (M-integer (expression-value expression) state) (removebinding (variable expression) state))))

; M-state-declaration
(define M-state-declaration
  (lambda (expression state)
    (cond
      ((null? (cddr expression)) (addbinding (variable expression) 'null state))
      ((declared? (variable expression) (car state)) (addbinding (variable expression) (M-integer (expression-value expression) state) (removebinding (variable expression) state)))
      (else (addbinding (variable expression) (M-integer (expression-value expression) state) state)))))

; Abstractions for M-state-assignment and declaration
(define variable cadr)
(define expression-value (lambda (expression) (caddr expression)))

; M-state-while
(define M-state-while
  (lambda (whilestate state)
    (cond
      ((M-boolean (condition whilestate) state)
       (M-state-while whilestate (M-state (body whilestate) state)))
      (else state))))

; Abstraction for M-state-while
(define condition cadr)
(define body caddr)

; M-state-return
(define M-state-return
  (lambda (returnstate state)
    (cond
      ((pair? (cadr returnstate))
       (if (isBool (cadr returnstate))
           (M-boolean (cadr returnstate) state)
           (M-integer (cadr returnstate) state)))
      ((eq? (cadr returnstate) '#t) #t)
      ((eq? (cadr returnstate) '#f) #f)
      (else (M-integer (cadr returnstate) state)))))

; M-state-if
(define M-state-if
  (lambda (ifstate state)
    (cond
      ((M-boolean (ifcondition ifstate) state)
       (M-state (ifbody ifstate) state))
      ((not (null? (cdddr ifstate))) (M-state (nextif ifstate) state))
      (else state))))

; Abstraction for M-state-if
(define ifcondition (lambda (ifstate) (cadr ifstate)))
(define ifbody (lambda (ifstate) (caddr ifstate)))
(define nextif (lambda (ifstate) (cadddr ifstate)))

; M-state
(define M-state
  (lambda (stmt state)
    (cond
      ((null? stmt) state)
      ((list? (operator stmt)) (M-state (remaining-stmts stmt) (M-state (first-stmt stmt) state)))
      ((eq? (operator stmt) 'var) (M-state-declaration stmt state))
      ((eq? (operator stmt) '=) (M-state-assignment stmt state))
      ((eq? (operator stmt) 'if) (M-state-if stmt state))
      ((eq? (operator stmt) 'while) (M-state-while stmt state))
      ((eq? (operator stmt) 'return) (M-state-return stmt state))
      (else (error 'stmt-not-defined)))))

; Abstraction for M-state
(define remaining-stmts cdr)
(define first-stmt car)

; list of helper methods

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
    (if
     (equal? (car lis) a) 0 (+ 1 (index a (cdr lis))))))
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
