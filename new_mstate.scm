(load "class_builder.scm")
(load "state-stack.scm")

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
      ((symbol? expr) (list (stack-get expr s) s))
      ((and (eq? (operator expr) '-)
            (null? (cddr expr))) (list (* (value (M_value (operand1 expr) s throw)) -1) s))
      ((eq? (operator expr) '+) (list (+ (value (M_value (operand1 expr) s throw))
                                         (value (M_value (operand2 expr) s throw))) s))
      ((eq? (operator expr) '-) (list (- (value (M_value (operand1 expr) s throw))
                                         (value (M_value (operand2 expr) s throw))) s))
      ((eq? (operator expr) '*) (list (* (value (M_value (operand1 expr) s throw))
                                         (value (M_value (operand2 expr) s throw))) s))
      ((eq? (operator expr) '/) (list (quotient (value (M_value (operand1 expr) s throw))
                                                (value (M_value (operand2 expr) s throw))) s))
      ((eq? (operator expr) '%) (list (remainder (value (M_value (operand1 expr) s throw))
                                                 (value (M_value (operand2 expr) s throw))) s))
      ((function-call? expr) (list (M_function-call-value (cadr expr) (cddr expr) s throw) (M_function-call-state (cadr expr) (cddr expr) s throw)))
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
                     ;(display s)
                     ;(display "\n")
                     (cond
                       ((null? stmts) (stack-pop s))
                       (else (loop (blocks stmts) (M_state (current-block stmts) s h return (lambda (v) (break (stack-pop v))) continue (lambda (v1 v2) (throw v1 (stack-pop v2))))))))))
      (loop stmts (stack-push (empty-state) s) h))))

; Gives the value returned by calling the given function with given actuals on state s
(define M_function-call-value
  (lambda (function-name actual s h throw)
    (call/cc
     (lambda (return)
       (if (has-func (car instance) function-name h)
           (M_begin (caddr (get-func (car instance) function-name h)) (actual-to-formal (list-to-value actual s throw) (cadr (get-func (car instance) function-name h)) (stack-push (empty-state) s)) h (lambda (v) (return (value v))) initial-break initial-continue throw)
           (M_begin (cadr (stack-get name s)) (actual-to-formal (list-to-value actual s throw) (car (stack-get name s)) (stack-push (empty-state) s) throw) h (lambda (v) (return (value v))) initial-break initial-continue throw))))))
    

;(define M_function-call-state
;  (lambda (name actual s throw)
;    (call/cc
;     (lambda (return)
;       (M_begin (cadr (stack-get name s)) (actual-to-formal (list-to-value actual s throw) (car (stack-get name s)) (stack-push (empty-state) s) throw) (lambda (v) (return (stack-pop (stack-pop (state v))))) initial-break initial-continue throw)))))

; Gives the state achieved after calling the given function with given actuals on state s
(define M_function-call-state
  (lambda (instance class function-name actual s h throw)
    (call/cc
     (lambda (return)
       (if (has-func class function-name h)
           (M_begin (caddr (get-func class function-name h)) (actual-to-formal (list-to-value actual s h throw) (cadr (get-func (car instance) function-name h)) (stack-push (empty-state) s)) h (lambda (v) (return (stack-pop (stack-pop (state v))))) initial-break initial-continue throw)
           (M_begin (cadr (stack-get name s)) (actual-to-formal (list-to-value actual s h throw) (car (stack-get name s)) (stack-push (empty-state) s) throw) h (lambda (v) (return (stack-pop (stack-pop (state v))))) initial-break initial-continue throw))))))

; Converts list of actuals to list of values (ints and bools)
(define list-to-value
  (lambda(actual s h throw)
    (if (null? actual)
        '()
        (cons (value (M_value (car actual) s h throw)) (list-to-value (cdr actual) s h throw)))))

; returns the (value, state) of a dot call
(define M_dot
  (lambda (instance var-name s h throw)
    (cond
      ((has-func (instance-type instance var-name h) (get-func (instance-type instance) var-name h))
      (else (get-var (instance-type instance) (instance-type instance) var-name (instance-vals instance h)))))))

(define get-class-type
  (lambda (object s h)
    (car (get-instance-of object s h))))

(define get-instance-of
  (lambda (object s h)
    (value (M_value object s h (initial-throw)))))

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
         

(define new?
  (lambda (stmt)
    (eq? 'new (car stmt))))

