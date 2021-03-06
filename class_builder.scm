(load "class-heap.scm")
(load "classParser.scm")

(define name cadr)
(define parent caddr)
(define class-body cadddr)

(define build
  (lambda (parsetree heap)
    (if (null? parsetree)
        heap
        (build (cdr parsetree) (add-class (build-class (car parsetree)) heap)))))

(define build-class
  (lambda (class)
    (list (name class) (parse-parent (parent class)) (parse-vars (class-body class)) (configure-funcs (name class) (parse-funcs (class-body class))) (add-to-constructors (get-var-assigns (class-body class)) (ensure-constructor (parse-constructors (class-body class)))))))

(define parse-parent
  (lambda (parent-line)
    (if (null? parent-line)
        parent-line
        (cadr parent-line))))

(define parse-vars
  (lambda (body)
    (cond
      ((null? body) '())
      ((eq? (caar body) 'var) (cons (cadar body) (parse-vars (cdr body))))
      (else (parse-vars (cdr body))))))

(define parse-funcs
  (lambda (body)
    (cond
      ((null? body) '())
      ((or (eq? (caar body) 'function) (eq? (caar body) 'static-function)) (cons (cdar body) (parse-funcs (cdr body))))
      (else (parse-funcs (cdr body))))))

(define configure-funcs
  (lambda (class-name funcs)
    (if (null? funcs)
        funcs
        (cons (append (car funcs) (cons class-name '())) (configure-funcs class-name (cdr funcs))))))

(define parse-constructors
  (lambda (body)
    (cond
      ((null? body) '())
      ((eq? (caar body) 'constructor) (cons (cdar body) (parse-constructors (cdr body))))
      (else (parse-constructors (cdr body))))))

(define ensure-constructor
  (lambda (constructors)
    (if (null? constructors)
        '((()()))
        constructors)))

(define add-to-constructors
  (lambda (var-assigns constructors)
    (if (null? constructors)
        '()
        (cons (cons (caar constructors) (cons (append var-assigns (cadar constructors)) '())) (add-to-constructors var-assigns (cdr constructors))))))

(define get-var-assigns
  (lambda (body)
    (cond
      ((null? body) '())
      ((eq? (caar body) 'var) (cons (cons '= (cdar body)) (get-var-assigns (cdr body))))
      (else (get-var-assigns (cdr body))))))

