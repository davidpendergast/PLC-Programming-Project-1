(define operator car)
(define operand1 cadr)
(define operand2 caddr)

(define M_boolean
  (lambda (expression s)
    (cond
      ((number? expression) expression)
      ((symbol? expression) (state-get expression s))
      ((eq? (operator expression) '==) (equal? (M_boolean (operand1 expression) s) (M_boolean (operand2 expression) s)))
      ((eq? (operator expression) '!=) (not (equal? (M_boolean (operand1 expression) s) (M_boolean (operand2 expression) s))))
      ((eq? (operator expression) '<) (< (M_boolean (operand1 expression) s) (M_boolean (operand2 expression) s)))
      ((eq? (operator expression) '>) (> (M_boolean (operand1 expression) s) (M_boolean (operand2 expression) s)))
      ((eq? (operator expression) '<=) (<= (M_boolean (operand1 expression) s) (M_boolean (operand2 expression) s)))
      ((eq? (operator expression) '>=) (>= (M_boolean (operand1 expression) s) (M_boolean (operand2 expression) s)))
      ((eq? (operator expression) '&&) (and (M_boolean (operand1 expression) s) (M_boolean (operand2 expression) s)))
      ((eq? (operator expression) '||) (or (M_boolean (operand1 expression) s) (M_boolean (operand2 expression) s)))
      ((eq? (operator expression) '!) (not (M_boolean (operand1 expression) s)))
      (else (error 'unknown "unknown expression")))))