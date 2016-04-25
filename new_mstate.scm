(load "class_builder.scm")

(define execfile
  (lambda (filename main-class)
    (outer-layer-interpret filename main-class (empty-state-stack) (build (parser filename) (empty-class-heap)))))

(define outer-layer-interpret
  (lambda (filename main-class s h)
    (interpret (caddr (get-func main-class 'main h)) s h)))