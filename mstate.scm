(load "simpleParser.scm")
(load "state.scm")
; mstate.scm

;(define interpret
;  (lambda (filename)
;    (runcode (parse filename) '('()'()) )))
    
;(define runcode
;  (lambda (tree s)
;    (cond
;      ((null? tree) s) ; shouldn't happen
;      (else "not finished")
;      
;      )))
 
(define operator car)
(define operand1 cadr)
(define operand2 caddr)

(define M_value
  (lambda (expression s)
    (cond
      ((number? expression) expression)
      ((symbol? expression) (state-get expression s))
      ((eq? (operator expression) '+) (+ (M_value (operand1 expression) s) (M_value (operand2 expression) s)))
      ((eq? (operator expression) '-) (- (M_value (operand1 expression) s) (M_value (operand2 expression) s)))
      ((eq? (operator expression) '*) (* (M_value (operand1 expression) s) (M_value (operand2 expression) s)))
      ((eq? (operator expression) '/) (quotient (M_value (operand1 expression) s) (M_value (operand2 expression) s)))
      ((eq? (operator expression) '%) (remainder (M_value (operand1 expression) s) (M_value (operand2 expression) s)))
      (else (error 'unknown "unknown expression")))))

(define M_boolean
  (lambda (expression s)
    (cond
      ((number? expression) expression)
      ((symbol? expression) (state-get expression s))
      ((eq? (operator expression) '==) (equal? (M_boolean (operand1 expression) s) (M_boolean (operand2 expression) s)))
      ((eq? (operator expression) '!=) (not (equal? (M_boolean (operand1 expression) s) (M_boolean (operand2 expression) s))))
      ((eq? (operator expression) '<) (< (M_boolean (operand1 expression) s) (M_boolean (operand2 expression) s)))
      ((eq? (operator expression) '>) (> (M_boolean (operand1 expression) s) (M_boolean (operand2 expression) s)))
      ((eq? (operator expression) '<=) (<= (M_boolean (operand1 expression) s) (M_boolean (operand2 expression) s)))
      ((eq? (operator expression) '>=) (>= (M_boolean (operand1 expression) s) (M_boolean (operand2 expression) s)))
      ((eq? (operator expression) '&&) (and (M_boolean (operand1 expression) s) (M_boolean (operand2 expression) s)))
      ((eq? (operator expression) '||) (or (M_boolean (operand1 expression) s) (M_boolean (operand2 expression) s)))
      ((eq? (operator expression) '!) (not (M_boolean (operand1 expression) s)))
      (else (error 'unknown "unknown expression")))))

(define M_state
  (lambda (stmt s)
    (cond
      ((declare? stmt) (M_declare (cadr stmt) s))
      ((declare_with_assign? stmt) (M_declare_with_assign (cadr stmt) (caddr stmt) s))
      ((assign? stmt) (M_assign (cadr stmt) (caddr stmt) s))
      (else '())
      )))

(define M_declare
  (lambda (variable s)
    (state-declare variable s)))

(define M_declare_with_assign
  (lambda (variable expression s)
    (state-assign variable (M_value expression (state-declare variable s)) (state-declare variable s))))

; takes statement of form (= variable expression), returns new state
(define M_assign
  (lambda (variable expression s)
    (if (state-declared? variable s)
        (state-assign variable (M_value expression s) s)
        (error variable "variable is not declared."))))

(define M_return
  (lambda (expression s)
    #f))

(define M_if_else
  (lambda (condition then-stmt else-stmt s)
    #f))

(define M_if
  (lambda (condition then-stmt s)
    #f))

(define M_while
  (lambda (condition loop-body s)
    #f))

;-------------------------------------------;
; Functions to determine types of statement |
;-------------------------------------------;
(define declare?
  (lambda (stmt)
    (if (eq? (length stmt) 2)
        (eq? 'var (car stmt))
        #f)))

(define declare_with_assign?
  (lambda (stmt)
    (if (eq? (length stmt) 3)
        (eq? 'var (car stmt))
        #f)))

(define assign?
  (lambda (stmt)
    (if (eq? (length stmt) 3)
        (eq? '= (car stmt))
        #f)))

(M_state '(var x) '(()()))
(M_state '(var x 10) '((y z)(15 40)))
(M_state '(= x 20) '((x) (10)))
(M_state '(= x 20) '((y x z) (0 () 6)))


  
;(M_boolean '(> 9 0) '(()()))