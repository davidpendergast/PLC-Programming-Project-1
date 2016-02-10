#lang racket
(load "parser.scm" "state.scm")
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
 
; takes a statement and a state, and returns the new state following the statement's execution.
(define M_state
  (lambda (stmt s)
    (cond
      ((declare? stmt) (M_declare (cadr stmt) s))
      ((declare_with_assign? stmt) (M_declare_with_assign (cadr stmt) (caddr stmt)))
      (else '())
      )))

(define declare?
  (lambda (stmt)
    (if (eq? (length stmt) 2)
        (eq? 'var (car stmt))
        #f)))

(define M_declare
  (lambda (variable s)
    (state-declare variable s)))

(define declare_with_assign?
  (lambda (stmt)
    (if (eq? (length stmt) 3)
        (eq? 'var (car stmt))
        #f)))
         
(define M_declare_with_assign
  (lambda (variable expression s)
    (state-assign variable (M_value expression (state-declare variable s)) (state-declare variable s))))

; takes statement of form (= variable expression), returns new state
(define M_assign
  (lambda (variable expression s)
    #f))

; takes statement of form ('return expression), returns new state
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

(define length
  (lambda (list)
    (if (null? list)
        0
        ((+ 1 (length (cdr list)))))))
