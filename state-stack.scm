; EECS 345 - Programming Project 3
; 
; David Pendergast
; Joel Kalos
; Kevin Nash

(load "state.scm")

; Returns ((()()))
(define empty-state-stack
  (lambda ()
    (cons (empty-state) '(()()) )))

(define stack-push
  (lambda (s stack)
    (cons s stack)))

(define stack-pop
  (lambda (stack)
    (cdr stack)))

(define stack-peek
  (lambda (stack)
    (car stack)))

; Removes the variable (and its associated value) from the state.
(define stack-remove
  (lambda (var stack)
    (cond
      ((null? stack) (error var "variable not declared"))
      ((state-declared? var (stack-peek stack)) (stack-push (state-remove var (stack-peek stack)) (stack-pop stack)))
      (else (stack-push (stack-peek stack) (stack-remove var (stack-pop stack)))))))

; Returns the value of var in the state.
(define stack-get
  (lambda (var stack)
    (cond
      ((null? stack) (error var "variable not declared"))
      ((state-declared? var (stack-peek stack)) (state-get var (stack-peek stack)))
      (else (stack-get var (stack-pop stack))))))

; Returns #t if var exists in the state, otherwise #f.
(define stack-declared?
  (lambda (var stack)
    (cond
      ((null? stack) #f)
      ((state-declared? var (stack-peek stack)) #t)
      (else (stack-declared? var (stack-pop stack))))))

; Returns #t if var has a value in s, otherwise #f.
(define stack-assigned?
  (lambda (var stack)
    (cond
      ((null? stack) #f)
      ((state-assigned? var (stack-peek stack)) #t)
      (else (stack-assigned? var (stack-pop stack))))))
      
; Adds new variable to the state with no value.
(define stack-declare
  (lambda (var stack)
    (if (state-declared? var (stack-peek stack))
        (error var "variable already declared")
        (stack-push (state-cons (list var '()) (stack-peek stack)) (stack-pop stack)))))

; Sets var's value in s. 
(define stack-assign
  (lambda (var val stack)
    (cond
      ((null? stack) (error var "variable not declared"))
      ((state-declared? var (stack-peek stack)) (stack-push (state-assign var val (stack-peek stack)) (stack-pop stack)))
      (else (stack-push (stack-peek stack) (stack-assign var val (stack-pop stack)))))))

;(define set-instance
;  (lambda (instance stack)
;    (stack-assign 'instance instance stack)))

;(define get-instance
;  (lambda (stack)
;    (stack-get 'instance stack)))
                           
  
