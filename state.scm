; EECS 345 - Programming Project 1
; 
; David Pendergast
; Joel Kalos
; Kevin Nash

(define first-var
  (lambda (s)
    (car (state-car s))))

(define first-val
  (lambda (s)
    (cadr (state-car s))))
    
(define state-cdr
  (lambda (s)
    (list (cdar s) (cdadr s))))

(define state-car
  (lambda (s)
    (list (caar s) (caadr s))))

(define state-cons
  (lambda (v s)
    (list (cons (car v) (car s)) (cons (cadr v) (cadr s)))))

; Removes the variable (and its associated value) from the state.
(define state-remove
  (lambda (var s)
    (cond
      ((null? (car s)) (error var "variable not declared"))
      ((eq? (first-var s) var) (state-cdr s))
      (else (state-cons (state-car s) (state-remove var (state-cdr s)))))))

; Returns the value of var in the state.
(define state-get
  (lambda (var s)
    (cond
      ((null? (car s)) (error var "variable not declared"))
      ((eq? (first-var s) var) (first-val s))
      (else (state-get var (state-cdr s))))))

; Returns #t if var exists in the state, otherwise #f.
(define state-declared?
  (lambda (var s)
    (cond
      ((null? (car s)) #f)
      ((eq? (first-var s) var) #t)
      (else (state-declared? var (state-cdr s))))))

; Returns #t if var has a value in s, otherwise #f.
(define state-assigned?
  (lambda (var s)
    (if (state-declared? var s)
        (not (null? (state-get var s)))
        (error var "variable not declared"))))
      
; Adds new variable to the state with no value.
(define state-declare
  (lambda (var s)
    (if (state-declared? var s)
        (error var "variable already declared")
        (state-cons (list var '()) s))))

; Sets var's value in s. 
(define state-assign
  (lambda (var val s)
    (cond
      ((null? (car s)) (error var "variable not declared"))
      ((eq? (first-var s) var) (list (car s) (cons val (cdadr s))))
      (else (state-cons (state-car s) (state-assign var val (state-cdr s)))))))

; Returns (()())
(define empty-state
  (lambda ()
    '(()())))
    