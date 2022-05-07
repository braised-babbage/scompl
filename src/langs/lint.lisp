(in-package #:scompl)

(defstruct (int-node (:constructor int-node (value)))
  value)
(defstruct (prim-node (:constructor prim-node (op args)))
  op
  args)

(defstruct (program (:constructor program (info body)))
  info
  body)

(defclass lint-interpreter ()
  ())

(defmethod interpret-expr ((interp lint-interpreter) env expr)
  (flet ((%interp (x)
	   (interpret-expr interp env x)))    
    (trivia:match expr
      ((int-node (value v))
       v)
      ((prim-node (op 'read))
       (let ((v (read)))
	 (check-type v integer)
	 v))
      ((prim-node (op '-) (args (list e)))
       (- (%interp e)))
      ((prim-node (op '+) (args (list e1 e2)))
       (+ (%interp e1) (%interp e2)))
      ((prim-node (op '-) (args (list e1 e2)))
       (- (%interp e1) (%interp e2)))
      (_ (error "Unexpected expression ~A" expr)))))
