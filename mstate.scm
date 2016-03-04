; EECS 345 - Programming Project 2
; 
; David Pendergast
; Joel Kalos
; Kevin Nash

(load "simpleParser.scm")
(load "state-stack.scm")

; Defines the elements of an expression in prefix notation
(define block cdr)
(define operator car)
(define operand1 cadr)
(define operand2 caddr)
(define operand3 cadddr)

; Returns the numerical value of given expression
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

; Returns the boolean value of given expression
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

; Returns true if the expression is a condition or truth value
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

; Given the filename of a valid program, returns the return value of the program
(define execfile
  (lambda (filename)
    (interpret (parser filename) (empty-state-stack))))

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
                               (lambda (v) (error "throw not inside try")))))))))

; Returns the state which results from executing the given statement OR
; Returns a value if the statement simplifies to a value
(define M_state
  (lambda (stmt s return break continue throw)
    (cond
      ((declare? stmt) (M_declare (operand1 stmt) s))
      ((declare_with_assign? stmt) (M_declare_with_assign (operand1 stmt)
                                                          (operand2 stmt) s))
      ((assign? stmt) (M_assign (operand1 stmt) (operand2 stmt) s))
      ((if? stmt) (M_if (operand1 stmt) (operand2 stmt) s return break continue throw))
      ((if_with_else? stmt) (M_if_else (operand1 stmt) (operand2 stmt)
                                       (operand3 stmt) s return break continue throw))
      ((while? stmt) (M_while (operand1 stmt) (operand2 stmt) s return throw))
      ((return? stmt) (M_return (operand1 stmt) s return))
      ((begin? stmt) (M_begin (block stmt) s return break continue throw))
      ((try? stmt) (M_try (block stmt) s return break continue))
      ((try_with_finally? stmt) (M_try_with_finally (block stmt) s return break continue))
      ((break? stmt) (break s))
      ((continue? stmt) (continue s))
      ((throw? stmt) (throw (operand1 stmt) s))
      (else (error stmt "unknown statement")))))

;Begins a block of statements and returns the state following the block
(define M_begin
  (lambda (stmts s return break continue throw)
    (letrec ((loop (lambda (stmts s)
                     (cond
                       ((null? stmts) (stack-pop s))
                       (else (loop (cdr stmts)
                                   (M_state (car stmts) s return
                                            (lambda (v) (break (stack-pop v)))
                                            continue throw)))))))
      (loop stmts (stack-push (empty-state) s)))))

(define M_try
  (lambda (stmts s return break continue)
    (call/cc
     (lambda (throw)
       (M_state (car stmts) s return break continue
                (lambda (v1 v2) (throw (M_catch v1 (cdadr stmts)
                                                v2 return break continue))))))))

(define M_catch
  (lambda (e stmts s return break continue)
    (stack-pop (M_state (cadr stmts)
                        (stack-assign (caar stmts) e
                                      (stack-declare (caar stmts)
                                                     (stack-push s (empty-state))))))))

(define M_try_with_finally
  (lambda (stmts s return break continue)
    (call/cc
     (lambda (throw)
       (display stmts)
       (M_state (cadar (cddr stmts))
                (M_state (car stmts) s return break continue
                         (lambda (v1 v2) (throw (M_state (cadar (cddr stmts))
                                                         (M_catch v1 (cdadr stmts) v2 return break continue) return break continue)))) return break continue)))))



; Declares a variable
(define M_declare
  (lambda (variable s)
    (stack-declare variable s)))

; Declares a variable and assigns it a numerical or boolean value
(define M_declare_with_assign
  (lambda (var expr s)
    (if (condition? expr s)
        (stack-assign var (M_boolean expr (stack-declare var s))
                      (stack-declare var s))
        (stack-assign var (M_value expr (stack-declare var s))
                      (stack-declare var s)))))

; Assigns a numerical or boolean value to a variable
(define M_assign
  (lambda (var expr s)
    (if (condition? expr s)
        (stack-assign var (M_boolean expr s) s)
        (stack-assign var (M_value expr s) s))))

; Returns a numerical value, not a state
(define M_return
  (lambda (expr s return)
    (if (condition? expr s)
        (if (M_boolean expr s)
            (return 'true)
            (return 'false))
        (return (M_value expr s)))))

; Executes an if-else pair of statements according to the if condition
(define M_if_else
  (lambda (condition then-stmt else-stmt s return break continue throw)
    (if (M_boolean condition s)
        (M_state then-stmt s return break continue throw)
        (M_state else-stmt s return break continue throw))))

; Executes an if statement according to its condition
(define M_if
  (lambda (condition then-stmt s return break continue throw)
    (if (M_boolean condition s)
        (M_state then-stmt s return break continue throw)
        s)))

; Executes a while loop according to its condition
(define M_while
  (lambda (condition loop-body s return throw)
    (call/cc
     (lambda (break)
       (call/cc
        (lambda (continue)
          (letrec ((loop (lambda (loop-body s)
                           (if (M_boolean condition s)
                               (loop loop-body (M_state loop-body s return break (lambda (v) (continue (loop loop-body v))) throw))
                               s))))
            (loop loop-body s))))))))

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

;Returns true if given a try statement with no finally
(define try?
  (lambda (stmt)
    (if (eq? (length stmt) 3)
        (eq? 'try (operator stmt))
        #f)))

;Returns true if given a try statement with a finally
(define try_with_finally?
  (lambda (stmt)
    (if (eq? (length stmt) 4)
        (eq? 'try (operator stmt))
        #f)))

;Returns true if given a throw statement
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

; -----------
; State tests
; -----------
(M_state '(var x) (empty-state-stack) (lambda (v) v) (lambda () (error "not in a block")) (lambda () (error "not in a block")) (lambda () (error "not in a block")))
(M_state '(var x 10) '(((y z)(15 40))) (lambda (v) v) (lambda () (error "not in a block")) (lambda () (error "not in a block")) (lambda () (error "not in a block")))
(M_state '(var x true) '(((y z)(15 40))) (lambda (v) v) (lambda () (error "not in a block")) (lambda () (error "not in a block")) (lambda () (error "not in a block")))
(M_state '(= x 20) '(((x) (10))) (lambda (v) v) (lambda () (error "not in a block")) (lambda () (error "not in a block")) (lambda () (error "not in a block")))
(M_state '(= x 20) '(((y x z) (0 () 6))) (lambda (v) v) (lambda () (error "not in a block")) (lambda () (error "not in a block")) (lambda () (error "not in a block")))
(M_state '(while (< i 10) (= i (+ i x))) '(((i x)(0 3))) (lambda (v) v) (lambda () (error "not in a block")) (lambda () (error "not in a block")) (lambda () (error "not in a block")))
(M_state '(if (< x 2) (= x 2)) '(((x)(1))) (lambda (v) v) (lambda () (error "not in a block")) (lambda () (error "not in a block")) (lambda () (error "not in a block")))
(M_state '(if (>= x 2) (= x 7) (= x (+ x 1))) '(((x)(0))) (lambda (v) v) (lambda () (error "not in a block")) (lambda () (error "not in a block")) (lambda () (error "not in a block")))

; --------------------
; Language tests (P1)
; --------------------
(display "P1 test 0: ") (equal? (execfile "p1_tests/test0.txt") 100)
(display "P1 test 1: ") (equal? (execfile "p1_tests/test1.txt") 150)
(display "P1 test 2: ") (equal? (execfile "p1_tests/test2.txt") -4)
(display "P1 test 3: ") (equal? (execfile "p1_tests/test3.txt") 10)
(display "P1 test 4: ") (equal? (execfile "p1_tests/test4.txt") 16)
(display "P1 test 5: ") (equal? (execfile "p1_tests/test5.txt") 220)
(display "P1 test 6: ") (equal? (execfile "p1_tests/test6.txt") 5)
(display "P1 test 7: ") (equal? (execfile "p1_tests/test7.txt") 6)
(display "P1 test 8: ") (equal? (execfile "p1_tests/test8.txt") 10)
(display "P1 test 9: ") (equal? (execfile "p1_tests/test9.txt") 5)
(display "P1 test 10: ") (equal? (execfile "p1_tests/test10.txt") -39)
; When enabled, tests 11-14 should produce specific errors
;(display "P1 test 11: ") (execfile "p1_tests/test11.txt") ; variable not declared
;(display "P1 test 12: ") (execfile "p1_tests/test12.txt") ; variable not declared
;(display "P1 test 13: ") (execfile "p1_tests/test13.txt") ; variable not initialized
;(display "P1 test 14: ") (execfile "p1_tests/test14.txt") ; variable already declared 
(display "P1 test 15: ") (equal? (execfile "p1_tests/test15.txt") 'true)
(display "P1 test 16: ") (equal? (execfile "p1_tests/test16.txt") 100)
(display "P1 test 17: ") (equal? (execfile "p1_tests/test17.txt") 'false)
(display "P1 test 18: ") (equal? (execfile "p1_tests/test18.txt") 'true)
(display "P1 test 19: ") (equal? (execfile "p1_tests/test19.txt") 128)
(display "P1 test 20: ") (equal? (execfile "p1_tests/test20.txt") 12)
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
(display "P2 test 1: ") (equal? (execfile "p2_tests/test1.txt") 20)
(display "P2 test 2: ") (equal? (execfile "p2_tests/test2.txt") 164)
(display "P2 test 3: ") (equal? (execfile "p2_tests/test3.txt") 32)
(display "P2 test 4: ") (equal? (execfile "p2_tests/test4.txt") 2)
; When enabled, test 5 should produce a "variable already declared" error
;(display "P2 test 5: ") (execfile "p2_tests/test5.txt")
(display "P2 test 6: ") (equal? (execfile "p2_tests/test6.txt") 25)
(display "P2 test 7: ") (equal? (execfile "p2_tests/test7.txt") 21)
(display "P2 test 8: ") (equal? (execfile "p2_tests/test8.txt") 6)
(display "P2 test 9: ") (equal? (execfile "p2_tests/test9.txt") -1)
(display "P2 test 10: ") (equal? (execfile "p2_tests/test10.txt") 789)
; When enabled, tests 11-12 should produce a "variable already declared" error
;(display "P2 test 11: ") (execfile "p2_tests/test11.txt")
;(display "P2 test 12: ") (execfile "p2_tests/test12.txt")
;(display "P2 test 13: ") (execfile "p2_tests/test13.txt")
(display "P2 test 15: ") (equal? (execfile "p2_tests/test15.txt") 125)
