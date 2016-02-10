; state.scm

(define declare
  (lambda (var s)

(define assign
  (lambda (var value s)

(define get
  (lambda (var s)

(define assigned?
  (lambda (var s)

(define declared?
  (lambda (var s)
    
(define state-cdr
  (lambda (s)
    (list (cdar s) (cdadr s))))

(define state-car
  (lambda (s)
    (list (caar s) (caadr s))))

(define state-cons
  (lambda (v s)
    (list (cons (car v) (car s)) (cons (cadr v) (cadr s)))))

(define state-remove
  (lambda (var s)
    (cond
      ((null? (car s)) (error var "variable not declared"))
      ((eq? (caar s) var) (state-cdr s))
      (else (state-cons (state-car s) (state-remove var (state-cdr s)))))))