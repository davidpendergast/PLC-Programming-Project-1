(load "class_builder.scm")

(define execfile
  (lambda (filename main-class)
    (outer-layer-interpret filename main-class (empty-state-stack) (build (parser filename) (empty-class-heap)))))

(define outer-layer-interpret
  (lambda (filename main-class s h)
    (call/cc
     (lambda (return)
       (interpret (caddr (get-func main-class 'main h)) s h return)))))

(define interpret
  (lambda (parsetree s h return)
    (if (null? parsetree)
        (error "parse tree reached no return statement")
        (interpret (cdr parsetree)
                   (M_state (car parsetree) s h (lambda (v) (return (value v))) initial-break initial-continue initial-throw) h return))))

(define M_state
  (lambda (stmt s h return break continue throw)
    (cond
      ((declare? stmt) (M_declare (var stmt) s))
      ((declare_with_assign? stmt) (M_declare_with_assign (var stmt)
                                                          (val stmt) s throw))
      ((assign? stmt) (M_assign (var stmt) (val stmt) s throw))
      ((if? stmt) (M_if (condition stmt) (body stmt) s return break continue throw))
      ((if_with_else? stmt) (M_if_else (condition stmt) (body stmt)
                                       (else-stmt stmt) s return break continue throw))
      ((while? stmt) (M_while (condition stmt) (body stmt) s return throw))
      ((return? stmt) (M_return (var stmt) s return throw))
      ((begin? stmt) (M_begin (blocks stmt) s return break continue throw))
      ((try? stmt) (M_try (blocks stmt) s return break continue throw))
      ((try_with_finally? stmt) (M_try_with_finally (blocks stmt) s return break continue throw))
      ((break? stmt) (break s))
      ((continue? stmt) (continue s))
      ((throw? stmt) (throw (value (M_value (var stmt) s throw)) s))
      ((function-assign? stmt) (M_function-assign (cadr stmt) (caddr stmt) (cadddr stmt) s))
      ((function-call? stmt) (M_function-call-state (cadr stmt) (cddr stmt) s throw))
      (else (error stmt "unknown statement")))))

(define dot?
  (lambda (stmt)
    (eq? 'dot (car stmt))))
(define new?
  (lambda (stmt)
    (eq? 'new (car stmt))))