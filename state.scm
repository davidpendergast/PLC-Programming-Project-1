; state.scm

(define declare
  (lambda (var s)s))

(define assign
  (lambda (var value s)s))

(define assigned?
  (lambda (var s)s))

(define declared?
  (lambda (var s)s))



    
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

(define state-remove
  (lambda (var s)
    (cond
      ((null? (car s)) (error var "variable not declared"))
      ((eq? (first-var s) var) (state-cdr s))
      (else (state-cons (state-car s) (state-remove var (state-cdr s)))))))

(define state-get
  (lambda (var s)
    (cond
      ((null? (car s)) (error var "variable not declared"))
      ((eq? (first-var s) var) (first-val s))
      (else (state-get var (state-cdr s))))))