(load "class_builder.scm")
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
(define val caddr)
(define value car)
(define state cadr)
(define function-body cadr)
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

(define execfile
  (lambda (filename main-class)
    (outer-layer-interpret filename main-class (empty-state-stack) (build (parser filename) (empty-class-heap)))))

(define outer-layer-interpret
  (lambda (filename main-class s h)
    (call/cc
     (lambda (return)
       (interpret (caddr (get-func main-class 'main h)) s h return)))))

(define interpret
  (lambda (parsetree s h return)
    (if (null? parsetree)
        (error "parse tree reached no return statement")
        (interpret (cdr parsetree)
                   (M_state (car parsetree) s h (lambda (v) (return (value v))) initial-break initial-continue initial-throw) h return))))

(define M_value
  (lambda (expr s h throw)
    (cond
      ((number? expr) (list expr s))
      ((boolean? expr) (list expr s))
      ((eq? 'true expr) (list #t s))
      ((eq? 'false expr) (list #f s))
      ((eq? expr 'this) (list (stack-get (stack-get 'instance s) s) s))
      ((symbol? expr) (list (stack-get expr s) s))
      ((and (eq? (operator expr) '-)
            (null? (cddr expr))) (list (* (value (M_value (operand1 expr) s h throw)) -1) s))
      ((eq? (operator expr) '+) (list (+ (value (M_value (operand1 expr) s h throw))
                                         (value (M_value (operand2 expr) s h throw))) s))
      ((eq? (operator expr) '-) (list (- (value (M_value (operand1 expr) s h throw))
                                         (value (M_value (operand2 expr) s h throw))) s))
      ((eq? (operator expr) '*) (list (* (value (M_value (operand1 expr) s h throw))
                                         (value (M_value (operand2 expr) s h throw))) s))
      ((eq? (operator expr) '/) (list (quotient (value (M_value (operand1 expr) s h throw))
                                                (value (M_value (operand2 expr) s h throw))) s))
      ((eq? (operator expr) '%) (list (remainder (value (M_value (operand1 expr) s h throw))
                                                 (value (M_value (operand2 expr) s h throw))) s))
      ((function-call? expr) (list (M_function-call-value (cadr expr) (cddr expr) s h throw) (M_function-call-state (cadr expr) (cddr expr) s h throw)))
      ((new? expr) (list (M_new (cdr expr) h) s))
      ((dot? expr) (list (value (M_dot (get-instance-of (cadr expr) s h) (caddr expr) (cadr expr) s h throw)) (state (M_dot (get-instance-of (cadr expr) s h) (caddr expr) (cadr expr) s h throw))))
      ((and (eq? (length expr) 2) (symbol? (car expr))) (list expr s))
      (else (error "unknown expression" expr)))))

(define M_state
  (lambda (stmt s h return break continue throw)
    (cond
      ((declare? stmt) (M_declare (var stmt) s))
      ((declare_with_assign? stmt) (M_declare_with_assign (var stmt) (val stmt) s h throw))
      ((assign? stmt) (M_assign (var stmt) (val stmt) s h throw))

      ((if? stmt) (M_if (condition stmt) (body stmt) s h return break continue throw))
      ((if_with_else? stmt) (M_if_else (condition stmt) (body stmt) (else-stmt stmt) s h return break continue throw))
      
      ((while? stmt) (M_while (condition stmt) (body stmt) s h return throw))
      ((return? stmt) (M_return (var stmt) s h return throw))
      ((begin? stmt) (M_begin (blocks stmt) s h return break continue throw))
      
      ((try? stmt) (M_try (blocks stmt) s h return break continue throw))
      ((try_with_finally? stmt) (M_try_with_finally (blocks stmt) s h return break continue throw))

      ((break? stmt) (break s))
      ((continue? stmt) (continue s))
      ((throw? stmt) (throw (value (M_value (var stmt) s h throw)) s))

      ((function-assign? stmt) (M_function-assign (cadr stmt) (caddr stmt) (cadddr stmt) s h))
      ((function-call? stmt) (M_function-call-state (cadr stmt) (cddr stmt) s h throw))
      (else (error stmt "unknown statement")))))

; Given a code block,
; returns the state following execution of the code block
(define M_begin
  (lambda (stmts s h return break continue throw)
    (letrec ((loop (lambda (stmts s h)
                     ;(display (M_state (current-block stmts) s h return break continue throw))
                     ;(display "\n")
                     (cond
                       ((null? stmts) (stack-pop s))
                       (else (loop (blocks stmts) (M_state (current-block stmts) s h return (lambda (v) (break (stack-pop v))) continue (lambda (v1 v2) (throw v1 (stack-pop v2)))) h))))))
      (loop stmts (stack-push (empty-state) s) h))))

; Given a statement that can be evaluated,
; returns the value of the statement
(define M_return
  (lambda (expr s h return throw)
    (if (condition? expr s)
        (if (value (M_boolean expr s h throw))
            (return (list 'true s))
            (return (list 'false s)))
        (return (M_value expr s h throw)))))

; Given a variable and an expression,
; returns the state after changing the value of the variable
(define M_assign
  (lambda (var expr s h throw)
    (cond
      ((and (dot? var) (eq? (cadr var) 'this)) (stack-assign (stack-get (cadr var) s) (set-instance-var (get-instance-of (stack-get (cadr var) s) s h) (caddr var) (val-assign expr s h throw) h) s))
      ((dot? var) (stack-assign (cadr var) (set-instance-var (get-instance-of (cadr var) s h) (caddr var) (val-assign expr s h throw) h) s))
      (else (if (condition? expr s)
                (stack-assign var (value (M_boolean expr s h throw)) s)
                (stack-assign var (value (M_value expr s h throw)) (state (M_value expr s h throw))))))))

(define val-assign
  (lambda (expr s h throw)
    (if (condition? expr s)
        (value (M_boolean expr s h throw))
        (value (M_value expr s h throw)))))

; Given a variable and an expression,
; returns the state after adding the initialized variable to the state
(define M_declare_with_assign
  (lambda (var expr s h throw)
    (if (condition? expr s)
        (stack-assign var (value (M_boolean expr (stack-declare var s) throw))
                      (stack-declare var s))
        (stack-assign var (value (M_value expr (stack-declare var s) h throw))
                      (stack-declare var s)))))

; Gives the value returned by calling the given function with given actuals on state s
(define M_function-call-value
  (lambda (name actual stack h throw)
    (call/cc
     (lambda (return)
       (letrec ([function-name (value (M_value name stack h throw))]
                [s (state (M_value name stack h throw))]
                [instance (stack-get (stack-get 'instance s) s)])
         (if (has-func (car instance) function-name h)
             (M_begin (caddr (get-func (car instance) function-name h)) (actual-to-formal (list-to-value actual s h throw) (cadr (get-func (car instance) function-name h)) (stack-push (empty-state) s) h throw) h (lambda (v) (return (value v))) initial-break initial-continue throw)
             (M_begin (cadr (stack-get function-name s)) (actual-to-formal (list-to-value actual s throw) (car (stack-get function-name s)) (stack-push (empty-state) s) h throw) h (lambda (v) (return (value v))) initial-break initial-continue throw)))))))
    

;(define M_function-call-state
;  (lambda (name actual s throw)
;    (call/cc
;     (lambda (return)
;       (M_begin (cadr (stack-get name s)) (actual-to-formal (list-to-value actual s throw) (car (stack-get name s)) (stack-push (empty-state) s) throw) (lambda (v) (return (stack-pop (stack-pop (state v))))) initial-break initial-continue throw)))))

; Gives the state achieved after calling the given function with given actuals on state s
(define M_function-call-state
  (lambda (name actual stack h throw)
    (call/cc
     (lambda (return)
       (letrec ([function-name (value (M_value name stack h throw))]
                [s (state (M_value name stack h throw))]
                [instance (stack-get (stack-get 'instance s) s)])
         (if (has-func (car instance) function-name h)
             (M_begin (caddr (get-func (car instance) function-name h)) (actual-to-formal (list-to-value actual s h throw) (cadr (get-func (car instance) function-name h)) (stack-push (empty-state) s) h throw) h (lambda (v) (return (stack-pop (stack-pop (state v))))) initial-break initial-continue throw)
             (M_begin (cadr (stack-get function-name s)) (actual-to-formal (list-to-value actual s throw) (car (stack-get function-name s)) (stack-push (empty-state) s) h throw) h (lambda (v) (return (stack-pop (stack-pop (state v))))) initial-break initial-continue throw)))))))

; Converts list of actuals to list of values (ints and bools)
(define list-to-value
  (lambda(actual s h throw)
    (if (null? actual)
        '()
        (cons (value (M_value (car actual) s h throw)) (list-to-value (cdr actual) s h throw)))))

; Places formal vars with associated actual values into the state.
(define actual-to-formal
  (lambda (actual formal s h throw)
    (cond
      ((and (null? formal) (null? actual)) s)
      ((not (equal? (null? formal) (null? actual))) (error "Actual and formal argument lists differ in length"))
      (else (actual-to-formal (cdr actual) (cdr formal) (M_declare_with_assign (car formal) (car actual) s h throw) h throw)))))

; returns the (value, state) of a dot call
(define M_dot
  (lambda (instance var-name instance-name s h throw)
    (if (has-func (get-class-type instance s h) var-name h)
        (list (car (get-func (get-class-type instance s h) var-name h)) (stack-assign 'instance instance-name (stack-declare 'instance s)))
        (list (get-var (get-class-type instance s h) (get-class-type instance s h) var-name (instance-vals (get-instance-of instance s h)) h) s))))

(define get-class-type
  (lambda (object s h)
    (car (get-instance-of object s h))))

(define get-instance-of
  (lambda (object s h)
    (value (M_value object s h initial-throw))))

(define dot?
  (lambda (stmt)
    (eq? 'dot (car stmt))))

; takes (A v1 v2...) -> outputs an instance
(define M_new
  (lambda (stmt h)
    (construct-instance (list (car stmt) ()) (car stmt) h)))

(define construct-instance
  (lambda (instance current-class-name h)
    (if (null? current-class-name)
        instance
        (add-var-layer-to-instance (construct-instance instance (get-parent current-class-name h) h) (reverse (cadr (call-constructor (get-constructor current-class-name h) (empty-state)))))))) 
        
(define call-constructor
  (lambda (constructor state)
    (cond
      ((null? constructor) state)
      ((eq? (length (car constructor)) 2) (call-constructor (cdr constructor) (state-declare (cadar constructor) state)))
      (else (call-constructor (cdr constructor) (state-assign (cadar constructor) (caddar constructor) (state-declare (cadar constructor) state)))))))
         
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

; Returns true if stmt is a function assign statement.
(define function-assign?
  (lambda (stmt)
    (if (eq? (length stmt) 4)
        (eq? 'function (operator stmt))
        #f
        )))

; Returns true if stmt is a function call.
(define function-call?
  (lambda (stmt)
    (eq? (operator stmt) 'funcall)))

(define new?
  (lambda (stmt)
    (eq? 'new (car stmt))))

(execfile "p4_tests/test1.txt" 'A)
(execfile "p4_tests/test2.txt" 'A)
(execfile "p4_tests/test3.txt" 'A)
(execfile "p4_tests/test4.txt" 'A)