(define empty-class-heap
  (lambda ()
    '()))

(define add-class
  (lambda (class heap)
    (cons class heap)))

(define find-class
  (lambda (name heap)
    (cond
      ((null? heap) (error "class does not exist"))
      ((eq? (caar heap) name) (car heap))
      (else (find-class name (cdr heap))))))

(define get-parent
  (lambda (name heap)
    (cadr (find-class name heap))))

(define find-vars
  (lambda (name heap)
    (caddr (find-class name heap))))

(define find-var
  (lambda (var-name var-vals vars)
    (cond
      ((null? var-vals) "DOESNOTEXIST")
      ((eq? (car vars) var-name) (car var-vals))
      (else (find-var var-name (cdr var-vals) (cdr vars))))))

(define find-set-var
  (lambda (var-name val var-vals vars)
    (cond
      ((null? var-vals) (error "wat"))
      ((eq? (car vars) var-name) (cons val (cdr var-vals)))
      (else (cons (car var-vals) (find-set-var var-name val (cdr var-vals) (cdr vars)))))))

(define find-funcs
  (lambda (name heap)
    (cadddr (find-class name heap))))

(define find-func
  (lambda (func-name funcs)
    (cond
      ((null? funcs) '())
      ((eq? func-name (caar funcs)) (car funcs))
      (else (find-func func-name (cdr funcs))))))

; returns (name )
(define get-func
  (lambda (class-name func-name heap)
    (cond
      ((null? class-name) (error "function does not exist"))
      ((null? (find-func func-name (find-funcs class-name heap))) (get-func (get-parent class-name heap) func-name heap))
      (else (find-func func-name (find-funcs class-name heap))))))

(define has-func
  (lambda (class-name func-name heap)
    (cond
      ((null? class-name) #f)
      ((null? (find-func func-name (find-funcs class-name heap))) (has-func (get-parent class-name heap) func-name heap))
      (else #t))))

(define get-var
  (lambda (class-name instance-name var-name var-vals heap)
    (cond
      ((null? class-name) (error "field does not exist"))
      ((not (eq? class-name instance-name)) (get-var class-name (get-parent instance-name heap) var-name (cdr var-vals) heap))
      ((eq? (find-var var-name (car var-vals) (find-vars class-name heap)) "DOESNOTEXIST") (get-var (get-parent class-name heap) (get-parent instance-name heap) var-name (cdr var-vals) heap))
      (else (find-var var-name (car var-vals) (find-vars class-name heap))))))

(define set-instance-var
  (lambda (instance var-name val heap)
    (list (car instance) (set-var (car instance) (car instance) var-name val (cdr instance) heap))))

;returns var-vals of an instance
(define set-var
  (lambda (class-name instance-name var-name val var-vals heap)
    (cond
      ((null? class-name) (error "function does not exist"))
      ((not (eq? class-name instance-name)) (cons (car var-vals) (set-var class-name (get-parent instance-name heap) var-name val (cdr var-vals) heap)))
      ((eq? (find-var var-name (car var-vals) (find-vars class-name heap)) "DOESNOTEXIST") (cons (car var-vals) (set-var (get-parent class-name heap) (get-parent instance-name heap) var-name val (cdr var-vals) heap)))
      (else (cons (find-set-var var-name val (car var-vals) (find-vars class-name heap)) (cdr var-vals))))))

;returns list of instructions in constructor
(define get-constructor
  (lambda (class-name h)
    (cadar (class-constructors (find-class class-name h))))) ; returning first constructor for now

; (A ((3 4)(2 1 1)(2))) --> A
(define instance-type
  (lambda (instance)
    (car instance)))
; (A ((3 4))) --> (A (()(3 4)))
(define add-var-layer-to-instance
  (lambda (instance layer)
    (cons (car instance) (cons (cons layer (cadr instance)) '()))))

; (A ((3 4)(2 1 1)(2))) --> ((3 4)(2 1 1)(2))
(define instance-vals
  (lambda (instance)
    (cadr instance)))

(define class-constructors
  (lambda (class)
    (car (cddddr class))))

(define class-parent
  (lambda (class)
    (cadr class)))
  
  