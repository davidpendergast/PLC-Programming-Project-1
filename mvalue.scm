(define operator car)
(define operand1 cadr)
(define operand2 caddr)

(define M_value
  (lambda (expression s)
    (cond
      ((number? expression) expression)
      ((symbol? expression) (state-get expression s))
      ((eq? (operator expression) '+) (+ (M_value (operand1 expression) s) (M_value (operand2 expression) s)))
      ((eq? (operator expression) '-) (- (M_value (operand1 expression) s) (M_value (operand2 expression) s)))
      ((eq? (operator expression) '*) (* (M_value (operand1 expression) s) (M_value (operand2 expression) s)))
      ((eq? (operator expression) '/) (quotient (M_value (operand1 expression) s) (M_value (operand2 expression) s)))
      ((eq? (operator expression) '%) (remainder (M_value (operand1 expression) s) (M_value (operand2 expression) s)))
      (else (error 'unknown "unknown expression")))))