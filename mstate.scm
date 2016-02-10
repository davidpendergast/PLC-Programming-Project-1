(load "parser.scm")
#lang racket
; mstate.scm

(define interpret
  (lambda (filename)
    (execute (parse filename) '(()()) )))
    
(define execute
  (lambda (tree s)
    (cond
      ((null? tree) s) ; shouldn't happen
      ()
 
; takes a statement and a state, and returns the new state following the statement's execution.
(define M_state
  (lambda (stmt s)))

(define M_declare
  (lambda (variable s)))

(define M_declare_with_assign
  (lambda (variable expression s)))

; takes statement of form (= variable expression), returns new state
(define M_assign
  (lambda (variable expression s)))

; takes statement of form ('return expression), returns new state
(define M_return
  (lambda (expression s)))

(define M_if_else
  (lambda (condition then-statement else-statement s)))

(define M_if
  (lambda (condition then-statement s)))

(define M_while
  (lambda (condition loop-body s)))
