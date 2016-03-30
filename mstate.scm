; EECS 345 - Programming Project 2
; 
; David Pendergast
; Joel Kalos
; Kevin Nash

(load "functionParser.scm")
;(load "simpleParser.scm")
(load "state-stack.scm")

; -----------
;   Aliases
; -----------

; Parts of an expression in prefix notation, e.g. (+ 1 2)
(define operator car)   ; +
(define operand1 cadr)  ; 1
(define operand2 caddr) ; 2
; A variable
(define var cadr)
; An expression that can be evaluated
(define value caddr)

(define function-body cdr)
; Code blocks following begin or try
(define blocks cdr)
; Current (first) code block in blocks
(define current-block car)
; Condition before a block, such as a while loop or if statement
(define condition cadr)
; Body of a loop or the code block following an if statement
(define body caddr)
; Code block following an else statement
(define else-stmt cadddr)
; Code block following a try
(define try-block car)
; Exception and block following a catch
(define catch cdadr)
; Block following a catch
(define catch-block cadr)
; Exception of a catch statement
(define exception caar)
; Block following a finally
(define (finally-block x) (cadar (cddr x)))

(define initial-break (lambda (v) (error "break not in a loop")))
(define initial-throw (lambda (v1 v2) (error "throw not inside try")))
(define initial-continue (lambda (v) (error "continue not in a loop")))
(define initial-return (lambda (v) (error "return outside of function")))

; --------------------
;   Runner functions
; --------------------

; Given the filename of a valid program, returns the return value of the program
(define execfile
  (lambda (filename)
    (interpret (function-body (stack-get 'main (outer-layer-interpret (parser filename) (empty-state-stack)))) (outer-layer-interpret (parser filename) (empty-state-stack)))))

; Returns the state after collecting the global functions.
(define outer-layer-interpret
  (lambda (parsetree s)
    (if (null? parsetree)
        s
        (outer-layer-interpret (cdr parsetree) (M_state (car parsetree) s initial-return initial-break initial-continue initial-throw)))))

; Given a parse tree, returns the return value of the program
(define interpret
  (lambda (parsetree s)
    (call/cc
     (lambda (return)
       (if (null? parsetree)
           (error "parse tree reached no return statement")
           (interpret (cdr parsetree)
                      (M_state (car parsetree) s return
                               (lambda (v) (error "break not in a loop"))
                               (lambda (v) (error "continue not in a loop"))
                               (lambda (v1 v2) (error "throw not inside try")))))))))

; ---------------
;   M functions
; ---------------

; Given a arithmetic expression, returns its numerical value
(define M_value
  (lambda (expr s)
    (cond
      ((number? expr) expr)
      ((boolean? expr) expr)
      ((eq? 'true expr) #t)
      ((eq? 'false expr) #f)
      ((symbol? expr) (stack-get expr s))
      ((and (eq? (operator expr) '-)
            (null? (cddr expr))) (* (M_value (operand1 expr) s) -1))
      ((eq? (operator expr) '+) (+ (M_value (operand1 expr) s)
                                   (M_value (operand2 expr) s)))
      ((eq? (operator expr) '-) (- (M_value (operand1 expr) s)
                                   (M_value (operand2 expr) s)))
      ((eq? (operator expr) '*) (* (M_value (operand1 expr) s)
                                   (M_value (operand2 expr) s)))
      ((eq? (operator expr) '/) (quotient (M_value (operand1 expr) s)
                                          (M_value (operand2 expr) s)))
      ((eq? (operator expr) '%) (remainder (M_value (operand1 expr) s)
                                           (M_value (operand2 expr) s)))
      (else (error "unknown expression" expr)))))

; Given a logical expression, returns its boolean value
(define M_boolean
  (lambda (expr s)
    (cond
      ((number? expr) expr)
      ((boolean? expr) expr)
      ((eq? 'true expr) #t)
      ((eq? 'false expr) #f)
      ((symbol? expr) (stack-get expr s))
      ((eq? (operator expr) '==) (equal? (M_value (operand1 expr) s)
                                         (M_value (operand2 expr) s)))
      ((eq? (operator expr) '!=) (not (equal? (M_value (operand1 expr) s)
                                              (M_value (operand2 expr) s))))
      ((eq? (operator expr) '<) (< (M_value (operand1 expr) s)
                                   (M_value (operand2 expr) s)))
      ((eq? (operator expr) '>) (> (M_value (operand1 expr) s)
                                   (M_value (operand2 expr) s)))
      ((eq? (operator expr) '<=) (<= (M_value (operand1 expr) s)
                                     (M_value (operand2 expr) s)))
      ((eq? (operator expr) '>=) (>= (M_value (operand1 expr) s)
                                     (M_value (operand2 expr) s)))
      ((eq? (operator expr) '&&) (and (M_boolean (operand1 expr) s)
                                      (M_boolean (operand2 expr) s)))
      ((eq? (operator expr) '||) (or (M_boolean (operand1 expr) s)
                                     (M_boolean (operand2 expr) s)))
      ((eq? (operator expr) '!) (not (M_boolean (operand1 expr) s)))
      (else (error "unknown expression" expr)))))

; Given a statement,
; returns the state which results from executing the given statement
(define M_state
  (lambda (stmt s return break continue throw)
    (cond
      ((declare? stmt) (M_declare (var stmt) s))
      ((declare_with_assign? stmt) (M_declare_with_assign (var stmt)
                                                          (value stmt) s))
      ((assign? stmt) (M_assign (var stmt) (value stmt) s))
      ((if? stmt) (M_if (condition stmt) (body stmt) s return break continue throw))
      ((if_with_else? stmt) (M_if_else (condition stmt) (body stmt)
                                       (else-stmt stmt) s return break continue throw))
      ((while? stmt) (M_while (condition stmt) (body stmt) s return throw))
      ((return? stmt) (M_return (var stmt) s return))
      ((begin? stmt) (M_begin (blocks stmt) s return break continue throw))
      ((try? stmt) (M_try (blocks stmt) s return break continue throw))
      ((try_with_finally? stmt) (M_try_with_finally (blocks stmt) s return break continue throw))
      ((break? stmt) (break s))
      ((continue? stmt) (continue s))
      ((throw? stmt) (throw (var stmt) s))
      ((function-assign? stmt) (M_function-assign (cadr stmt) (caddr stmt) (cadddr stmt) s))
      (else (error stmt "unknown statement")))))

; Given a code block,
; returns the state following execution of the code block
(define M_begin
  (lambda (stmts s return break continue throw)
    (letrec ((loop (lambda (stmts s)
                     (cond
                       ((null? stmts) (stack-pop s))
                       (else (loop (blocks stmts) (M_state (current-block stmts) s return (lambda (v) (break (stack-pop v))) continue (lambda (v1 v2) (throw v1 (stack-pop v2))))))))))
      (loop stmts (stack-push (empty-state) s)))))

; Given a try statement,
; returns the state following execution of the statement
(define M_try
  (lambda (stmts s return break continue throw)
    (call/cc
     (lambda (throw2)
       (M_begin (try-block stmts) s return break continue (lambda (v1 v2) (throw2 (M_catch v1 (catch stmts) v2 return break continue throw))))))))

; Given an exception value and a catch code block,
; returns the state following execution of the statement
(define M_catch
  (lambda (e stmts s return break continue throw)
    (stack-pop (M_begin (catch-block stmts) (stack-assign (exception stmts) e (stack-declare (exception stmts) (stack-push (empty-state) s))) return break continue throw))))

; Given a try statement including a finally statement,
; returns the state following execution of the statements
(define M_try_with_finally
  (lambda (stmts s return break continue throw)
    (call/cc
     (lambda (throw2)
       (M_begin (finally-block stmts)
                (M_begin (try-block stmts) s return break continue
                         (lambda (v1 v2) (throw2 (M_begin (finally-block stmts)
                                                         (M_catch v1 (catch stmts) v2 return break continue throw) return break continue throw)))) return break continue throw)))))

; Given a variable,
; returns the state after adding the variable to the state
(define M_declare
  (lambda (variable s)
    (stack-declare variable s)))

; Given a variable and an expression,
; returns the state after adding the initialized variable to the state
(define M_declare_with_assign
  (lambda (var expr s)
    (if (condition? expr s)
        (stack-assign var (M_boolean expr (stack-declare var s))
                      (stack-declare var s))
        (stack-assign var (M_value expr (stack-declare var s))
                      (stack-declare var s)))))

; Given a variable and an expression,
; returns the state after changing the value of the variable
(define M_assign
  (lambda (var expr s)
    (if (condition? expr s)
        (stack-assign var (M_boolean expr s) s)
        (stack-assign var (M_value expr s) s))))

; Given a statement that can be evaluated,
; returns the value of the statement
(define M_return
  (lambda (expr s return)
    (if (condition? expr s)
        (if (M_boolean expr s)
            (return 'true)
            (return 'false))
        (return (M_value expr s)))))

; Given a condition and two statements,
; returns the state following execution of the first statement if the condition
; is met or, otherwise, the state following execution of the second statement
(define M_if_else
  (lambda (condition then-stmt else-stmt s return break continue throw)
    (if (M_boolean condition s)
        (M_state then-stmt s return break continue throw)
        (M_state else-stmt s return break continue throw))))

; Given a condition and a statement,
; returns the state following execution of the statement if the condition
; is met or the original state if the condition is not met
(define M_if
  (lambda (condition then-stmt s return break continue throw)
    (if (M_boolean condition s)
        (M_state then-stmt s return break continue throw)
        s)))

; Given a condition and a code block
; returns the state following all necessary loops of the code block
(define M_while
  (lambda (condition loop-body s return throw)
    (call/cc
     (lambda (break)
       (call/cc
        (lambda (continue)
          (letrec ((loop (lambda (loop-body s)
                           ;(display s)(display "$$$")
                           (if (M_boolean condition s)
                               (loop loop-body (M_state loop-body s return break (lambda (v) (continue (loop loop-body (stack-pop v)))) throw))
                               s))))
            (loop loop-body s))))))))

(define M_function-assign
  (lambda (name args stmts s)
    (stack-assign name (list args stmts) (stack-declare name s))))

(define actual-to-formal
  (lambda (actual formal s)
    (if (null? formal)
      s
      (actual-to-formal (cdr actual) (cdr formal) (stack-assign (car formal) (M_value (car actual) s) (stack-declare (car formal) s))))))

(define M_function-call-value
  (lambda (name actual s)
    (call/cc
      (lambda (return)
        (M_begin (cdr (stack-get name)) (actual-to-formal actual (car (stack-get name)) (stack-push (empty-state) s)) (lambda (v) (return (car v))) initial-break initial-continue initial-throw)))))

(define M_function-call-state
  (lambda (name actual s)
    (call/cc
      (lambda (return)
        (M_begin (cdr (stack-get name)) (actual-to-formal actual (car (stack-get name)) (stack-push (empty-state) s)) (lambda (v) (return (stack-pop (cadr v)))) initial-break initial-continue initial-throw)))))


; -------------------------------------------------
;   Statement/expression identification functions  
; -------------------------------------------------

; Given a logical expression, returns true
; Given an arithmetic expression, returns false
(define condition?
  (lambda (expr s)
    (cond
      ((boolean? expr) #t)
      ((eq? 'true expr) #t)
      ((eq? 'false expr) #t)
      ((symbol? expr) (boolean? (stack-get expr s)))
      ((not (list? expr)) #f)
      (else (or (eq? (operator expr) '==) (eq? (operator expr) '!=)
                (eq? (operator expr) '<) (eq? (operator expr) '>)
                (eq? (operator expr) '<=) (eq? (operator expr) '>=)
                (eq? (operator expr) '&&) (eq? (operator expr) '||)
                (eq? (operator expr) '!))))))

; Returns true if given a statement that only declares a variable
(define declare?
  (lambda (stmt)
    (if (eq? (length stmt) 2)
        (eq? 'var (operator stmt))
        #f)))

; Returns true if given a statement that declares a variable and assigns a value
(define declare_with_assign?
  (lambda (stmt)
    (if (eq? (length stmt) 3)
        (eq? 'var (operator stmt))
        #f)))

; Returns true if given a statement that assigns a value to a declared variable
(define assign?
  (lambda (stmt)
    (if (eq? (length stmt) 3)
        (eq? '= (operator stmt))
        #f)))

; Returns true if given a return statement
(define return?
  (lambda (stmt)
    (if (eq? (length stmt) 2)
        (eq? 'return (operator stmt))
        #f)))

; Returns true if given a begin statement
(define begin?
  (lambda (stmt)
    (eq? 'begin (operator stmt))))

; Returns true if given a break statement
(define break?
  (lambda (stmt)
    (if (eq? (length stmt) 1)
        (eq? 'break (operator stmt))
        #f)))

; Returns true if given a continue statement
(define continue?
  (lambda (stmt)
    (if (eq? (length stmt) 1)
        (eq? 'continue (operator stmt))
        #f)))

; Returns true if given a try statement with no finally
(define try?
  (lambda (stmt)
    (if (eq? 'try (operator stmt))
        (eq? (length (cadddr stmt)) 0)
        #f)))

; Returns true if given a try statement with a finally
(define try_with_finally?
  (lambda (stmt)
    (if (eq? (length stmt) 4)
        (eq? 'try (operator stmt))
        #f)))

; Returns true if given a throw statement
(define throw?
  (lambda (stmt)
    (if (eq? (length stmt) 2)
        (eq? 'throw (operator stmt))
        #f)))

; Returns true if given an if statement that is NOT followed by an else
(define if?
  (lambda (stmt)
    (if (eq? (length stmt) 3)
        (eq? 'if (operator stmt))
        #f)))

; Returns true if given an if statement that is followed by an else
(define if_with_else?
  (lambda (stmt)
    (if (eq? (length stmt) 4)
        (eq? 'if (operator stmt))
        #f)))

; Returns true if a statement begins a while loop
(define while?
  (lambda (stmt)
    (if (eq? (length stmt) 3)
        (eq? 'while (operator stmt))
        #f)))

(define function-assign?
  (lambda (stmt)
    (if (eq? (length stmt) 4)
        (eq? 'function (operator stmt))
        #f
    )))

(define function-call?
  (lambda (stmt)
    (eq? (opperator stmt) 'funcall)))

; -----------
; State tests
; -----------

;(M_state '(var x) (empty-state-stack) (lambda (v) v) (lambda () (error "not in a block")) (lambda () (error "not in a block")) (lambda () (error "not in a block")))
;(M_state '(var x 10) '(((y z)(15 40))) (lambda (v) v) (lambda () (error "not in a block")) (lambda () (error "not in a block")) (lambda () (error "not in a block")))
;(M_state '(var x true) '(((y z)(15 40))) (lambda (v) v) (lambda () (error "not in a block")) (lambda () (error "not in a block")) (lambda () (error "not in a block")))
;(M_state '(= x 20) '(((x) (10))) (lambda (v) v) (lambda () (error "not in a block")) (lambda () (error "not in a block")) (lambda () (error "not in a block")))
;(M_state '(= x 20) '(((y x z) (0 () 6))) (lambda (v) v) (lambda () (error "not in a block")) (lambda () (error "not in a block")) (lambda () (error "not in a block")))
;(M_state '(while (< i 10) (= i (+ i x))) '(((i x)(0 3))) (lambda (v) v) (lambda () (error "not in a block")) (lambda () (error "not in a block")) (lambda () (error "not in a block")))
;(M_state '(if (< x 2) (= x 2)) '(((x)(1))) (lambda (v) v) (lambda () (error "not in a block")) (lambda () (error "not in a block")) (lambda () (error "not in a block")))
;(M_state '(if (>= x 2) (= x 7) (= x (+ x 1))) '(((x)(0))) (lambda (v) v) (lambda () (error "not in a block")) (lambda () (error "not in a block")) (lambda () (error "not in a block")))

; --------------------
; Language tests (P1)
; --------------------

;(display "P1 test 0: ") (equal? (execfile "p1_tests/test0.txt") 100)
;(display "P1 test 1: ") (equal? (execfile "p1_tests/test1.txt") 150)
;(display "P1 test 2: ") (equal? (execfile "p1_tests/test2.txt") -4)
;(display "P1 test 3: ") (equal? (execfile "p1_tests/test3.txt") 10)
;(display "P1 test 4: ") (equal? (execfile "p1_tests/test4.txt") 16)
;(display "P1 test 5: ") (equal? (execfile "p1_tests/test5.txt") 220)
;(display "P1 test 6: ") (equal? (execfile "p1_tests/test6.txt") 5)
;(display "P1 test 7: ") (equal? (execfile "p1_tests/test7.txt") 6)
;(display "P1 test 8: ") (equal? (execfile "p1_tests/test8.txt") 10)
;(display "P1 test 9: ") (equal? (execfile "p1_tests/test9.txt") 5)
;(display "P1 test 10: ") (equal? (execfile "p1_tests/test10.txt") -39)
; When enabled, tests 11-14 should produce specific errors
;(display "P1 test 11: ") (execfile "p1_tests/test11.txt") ; variable not declared
;(display "P1 test 12: ") (execfile "p1_tests/test12.txt") ; variable not declared
;(display "P1 test 13: ") (execfile "p1_tests/test13.txt") ; variable not initialized
;(display "P1 test 14: ") (execfile "p1_tests/test14.txt") ; variable already declared 
;(display "P1 test 15: ") (equal? (execfile "p1_tests/test15.txt") 'true)
;(display "P1 test 16: ") (equal? (execfile "p1_tests/test16.txt") 100)
;(display "P1 test 17: ") (equal? (execfile "p1_tests/test17.txt") 'false)
;(display "P1 test 18: ") (equal? (execfile "p1_tests/test18.txt") 'true)
;(display "P1 test 19: ") (equal? (execfile "p1_tests/test19.txt") 128)
;(display "P1 test 20: ") (equal? (execfile "p1_tests/test20.txt") 12)
; Tests 21-28 are expected to fail. The feature they test is not implemented.
;(display "P1 test 21: ") (equal? (execfile "p1_tests/test21.txt") 30)
;(display "P1 test 22: ") (equal? (execfile "p1_tests/test22.txt") 11)
;(display "P1 test 23: ") (equal? (execfile "p1_tests/test23.txt") 1106)
;(display "P1 test 24: ") (equal? (execfile "p1_tests/test24.txt") 12)
;(display "P1 test 25: ") (equal? (execfile "p1_tests/test25.txt") 16)
;(display "P1 test 26: ") (equal? (execfile "p1_tests/test26.txt") 72)
;(display "P1 test 27: ") (equal? (execfile "p1_tests/test27.txt") 21)
;(display "P1 test 28: ") (equal? (execfile "p1_tests/test28.txt") 164)

; --------------------
; Language tests (P2)
; --------------------

;(display "P2 test 1: ") (equal? (execfile "p2_tests/test1.txt") 20)
;(display "P2 test 2: ") (equal? (execfile "p2_tests/test2.txt") 164)
;(display "P2 test 3: ") (equal? (execfile "p2_tests/test3.txt") 32)
;(display "P2 test 4: ") (equal? (execfile "p2_tests/test4.txt") 2)
; When enabled, test 5 should produce a "variable not declared" error
;(display "P2 test 5: ") (execfile "p2_tests/test5.txt")
;(display "P2 test 6: ") (equal? (execfile "p2_tests/test6.txt") 25)
;(display "P2 test 7: ") (equal? (execfile "p2_tests/test7.txt") 21)
;(display "P2 test 8: ") (equal? (execfile "p2_tests/test8.txt") 6)
;(display "P2 test 9: ") (equal? (execfile "p2_tests/test9.txt") -1)
;(display "P2 test 10: ") (equal? (execfile "p2_tests/test10.txt") 789)
; When enabled, tests 11-12 should throw a "variable not declared" error
;(display "P2 test 11: ") (execfile "p2_tests/test11.txt")
;(display "P2 test 12: ") (execfile "p2_tests/test12.txt")
; When enabled, test 13 should produce a "break not in loop" error
;(display "P2 test 13: ") (execfile "p2_tests/test13.txt")
;(display "P2 test 14: ") (equal? (execfile "p2_tests/test14.txt") 12)
;(display "P2 test 15: ") (equal? (execfile "p2_tests/test15.txt") 125)
;(display "P2 test 16: ") (equal? (execfile "p2_tests/test16.txt") 110)
;(display "P2 test 17: ") (equal? (execfile "p2_tests/test17.txt") 2000400)
;(display "P2 test 18: ") (equal? (execfile "p2_tests/test18.txt") 101)
; When enabled, test 19 should produce a "throw not inside try" error
;(display "P2 test 19: ") (execfile "p2_tests/test19.txt")
; Test 20 is expected to fail. The feature it tests is not implemented.
;(display "P2 test 20: ") (equal? (execfile "p2_tests/test14.txt") 21)

;(execfile "test0.txt")
(outer-layer-interpret (parser "p3_tests/test0.txt") (empty-state-stack))
