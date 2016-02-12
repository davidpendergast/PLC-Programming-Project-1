; David Pendergast
; Joel Kalos
; Kevin Nash

(load "simpleParser.scm")
(load "state.scm")
 
(define operator car)
(define operand1 cadr)
(define operand2 caddr)

; returns the numerical value of given expression.
(define M_value
  (lambda (expression s)
    (cond
      ((number? expression) expression)
      ((boolean? expression) expression)
      ((eq? 'true expression) #t)
      ((eq? 'false expression) #f)
      ((symbol? expression) (state-get expression s))
      ((and (eq? (operator expression) '-) (null? (cddr expression))) (* (M_value (operand1 expression) s) -1))
      ((eq? (operator expression) '+) (+ (M_value (operand1 expression) s) (M_value (operand2 expression) s)))
      ((eq? (operator expression) '-) (- (M_value (operand1 expression) s) (M_value (operand2 expression) s)))
      ((eq? (operator expression) '*) (* (M_value (operand1 expression) s) (M_value (operand2 expression) s)))
      ((eq? (operator expression) '/) (quotient (M_value (operand1 expression) s) (M_value (operand2 expression) s)))
      ((eq? (operator expression) '%) (remainder (M_value (operand1 expression) s) (M_value (operand2 expression) s)))
      (else (error "unknown expression" expression)))))

; returns the boolean value of given expression.
(define M_boolean
  (lambda (expression s)
    (cond
      ((number? expression) expression)
      ((boolean? expression) expression)
      ((eq? 'true expression) #t)
      ((eq? 'false expression) #f)
      ((symbol? expression) (state-get expression s))
      ((eq? (operator expression) '==) (equal? (M_value (operand1 expression) s) (M_value (operand2 expression) s)))
      ((eq? (operator expression) '!=) (not (equal? (M_value (operand1 expression) s) (M_value (operand2 expression) s))))
      ((eq? (operator expression) '<) (< (M_value (operand1 expression) s) (M_value (operand2 expression) s)))
      ((eq? (operator expression) '>) (> (M_value (operand1 expression) s) (M_value (operand2 expression) s)))
      ((eq? (operator expression) '<=) (<= (M_value (operand1 expression) s) (M_value (operand2 expression) s)))
      ((eq? (operator expression) '>=) (>= (M_value (operand1 expression) s) (M_value (operand2 expression) s)))
      ((eq? (operator expression) '&&) (and (M_boolean (operand1 expression) s) (M_boolean (operand2 expression) s)))
      ((eq? (operator expression) '||) (or (M_boolean (operand1 expression) s) (M_boolean (operand2 expression) s)))
      ((eq? (operator expression) '!) (not (M_boolean (operand1 expression) s)))
      (else (error "unknown expression" expression)))))

(define condition?
  (lambda (expression s)
    (cond
      ((boolean? expression) #t)
      ((eq? 'true expression) #t)
      ((eq? 'false expression) #t)
      ((symbol? expression) (boolean? (state-get expression s)))
      ((not (list? expression)) #f)
      (else (or (eq? (operator expression) '==) (eq? (operator expression) '!=)
                (eq? (operator expression) '<) (eq? (operator expression) '>) (eq? (operator expression) '<=)
                (eq? (operator expression) '>=) (eq? (operator expression) '&&) (eq? (operator expression) '||)
                (eq? (operator expression) '!))))))
  
; takes the filename of a valid program, and returns that program's return value.
(define execfile
  (lambda (filename)
    (interpret (parser filename) '(()()))))

; takes a parse tree, returns the return value of the program. 
(define interpret
  (lambda (parsetree s)
    (cond
      ((number? s) s) ; return value reached
      ((boolean? s) s)
      ((null? parsetree) (error "parse tree reached no return statement."))
      (else (interpret (cdr parsetree) (M_state (car parsetree) s))))))

; returns the state which results from executing the given statement on the given state.
; if the statement results in a value being returned, then that value is returned instead
(define M_state
  (lambda (stmt s)
    (cond
      ((declare? stmt) (M_declare (cadr stmt) s))
      ((declare_with_assign? stmt) (M_declare_with_assign (cadr stmt) (caddr stmt) s))
      ((assign? stmt) (M_assign (cadr stmt) (caddr stmt) s))
      ((if? stmt) (M_if (cadr stmt) (caddr stmt) s))
      ((if_with_else? stmt) (M_if_else (cadr stmt) (caddr stmt) (cadddr stmt) s))
      ((while? stmt) (M_while (cadr stmt) (caddr stmt) s))
      ((return? stmt) (M_return (cadr stmt) s))
      (else (error stmt "unknown statement.")))))

(define M_declare
  (lambda (variable s)
    (state-declare variable s)))

(define M_declare_with_assign
  (lambda (variable expression s)
    (if (condition? expression s)
        (state-assign variable (M_boolean expression (state-declare variable s)) (state-declare variable s))
        (state-assign variable (M_value expression (state-declare variable s)) (state-declare variable s)))))

(define M_assign
  (lambda (variable expression s)
    (if (condition? expression s)
        (state-assign variable (M_boolean expression s) s)
        (state-assign variable (M_value expression s) s))))

; returns a value (number), not a state
(define M_return
  (lambda (expression s)
    (if (condition? expression s)
        (M_boolean expression s)
        (M_value expression s))))

(define M_if_else
  (lambda (condition then-stmt else-stmt s)
   (if (M_boolean condition s)
        (M_state then-stmt s)
        (M_state else-stmt s))))

(define M_if
  (lambda (condition then-stmt s)
    (if (M_boolean condition s)
        (M_state then-stmt s)
        s)))

(define M_while
  (lambda (condition loop-body s)
    (if (M_boolean condition s)
        (M_while condition  loop-body (M_state loop-body s))
        s)))

; the following functions determine what kind of statement they are passed.
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

(define return?
  (lambda (stmt)
    (if (eq? (length stmt) 2)
        (eq? 'return (car stmt))
        #f)))

(define if?
  (lambda (stmt)
    (if (eq? (length stmt) 3)
        (eq? 'if (car stmt))
        #f)))

(define if_with_else?
  (lambda (stmt)
    (if (eq? (length stmt) 4)
        (eq? 'if (car stmt))
        #f)))

(define while?
  (lambda (stmt)
    (if (eq? (length stmt) 3)
        (eq? 'while (car stmt))
        #f)))

; tests
(M_state '(var x) '(()()))
(M_state '(var x 10) '((y z)(15 40)))
(M_state '(var x true) '((y z)(15 40)))
(M_state '(= x 20) '((x) (10)))
(M_state '(= x 20) '((y x z) (0 () 6)))
(M_state '(while (< i 10) (= i (+ i x))) '((i x)(0 3)))
(M_state '(if (< x 2) (= x 2)) '((x)(1)))
(M_state '(if (>= x 2) (= x 7) (= x (+ x 1))) '((x)(0)))

; running the sample file.
;(execfile "testfile.txt")
;(M_state '(var x) '(()()))
;(M_state '(var x 10) '((y z)(15 40)))
;(M_state '(= x 20) '((x) (10)))
;(M_state '(= x 20) '((y x z) (0 () 6)))
;(M_state '(while (< i 10) (= i (+ i x))) '((i x)(0 3)))
;(M_state '(if (< x 2) (= x 2)) '((x)(1)))
;(M_state '(if (>= x 2) (= x 7) (= x (+ x 1))) '((x)(0)))

; tests
(display "Test 0: ") (equal? (execfile "test0.txt") 100)
(display "Test 1: ") (equal? (execfile "test1.txt") 150)
(display "Test 2: ") (equal? (execfile "test2.txt") -4)
(display "Test 3: ") (equal? (execfile "test3.txt") 10)
(display "Test 4: ") (equal? (execfile "test4.txt") 16)
(display "Test 5: ") (equal? (execfile "test5.txt") 220)
(display "Test 6: ") (equal? (execfile "test6.txt") 5)
(display "Test 7: ") (equal? (execfile "test7.txt") 6)
(display "Test 8: ") (equal? (execfile "test8.txt") 10)
(display "Test 9: ") (equal? (execfile "test9.txt") 5)
(display "Test 10: ") (equal? (execfile "test10.txt") -39)
; When enabled, tests 11-14 should produce specific errors
;(display "Test 11: ") (execfile "test11.txt") ; variable not declared
;(display "Test 12: ") (execfile "test12.txt") ; variable not declared
;(display "Test 13: ") (execfile "test13.txt") ; variable not initialized
;(display "Test 14: ") (execfile "test14.txt") ; variable already declared 
;(display "Test 15: ") (equal? (execfile "test15.txt") 'true)
(display "Test 16: ") (equal? (execfile "test16.txt") 100)
(display "Test 17: ") (equal? (execfile "test17.txt") 'false)
(display "Test 18: ") (equal? (execfile "test18.txt") 'true)
(display "Test 19: ") (equal? (execfile "test19.txt") 128)
(display "Test 20: ") (equal? (execfile "test20.txt") 12)
(display "Test 21: ") (equal? (execfile "test21.txt") 30)
(display "Test 22: ") (equal? (execfile "test22.txt") 11)
(display "Test 23: ") (equal? (execfile "test23.txt") 1106)
(display "Test 24: ") (equal? (execfile "test24.txt") 12)
(display "Test 25: ") (equal? (execfile "test25.txt") 16)
(display "Test 26: ") (equal? (execfile "test26.txt") 72)
(display "Test 27: ") (equal? (execfile "test27.txt") 21)
(display "Test 28: ") (equal? (execfile "test28.txt") 164)
