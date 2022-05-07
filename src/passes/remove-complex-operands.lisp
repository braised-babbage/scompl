(in-package #:scompl)


(defun rco-expr (expr)
  (trivia:match expr
    ((var-node) expr)
    ((int-node) expr)
    ((let-node (var var) (val val) (body body))
     (let-node var (rco-expr val) (rco-expr body)))
    ((prim-node (op op) (args args))
     (multiple-value-bind (atoms bindings)
	 (loop :for arg :in args
	       :for (%atom %bindings) := (multiple-value-list (rco-atom arg))
	       :collect %atom :into atoms
	       :append %bindings :into bindings
	       :finally (return (values atoms bindings)))
       (reduce (lambda (binding expr)
		 (let-node (car binding) (cdr binding) expr))
	       bindings
	       :initial-value (prim-node op atoms)
	       :from-end t)))))

(defun rco-atom (expr)
  (trivia:match expr
    ((var-node) expr)
    ((int-node) expr)
    ((let-node (var var) (val val) (body body))
     (multiple-value-bind (atom bindings)
	 (rco-atom body)
       (values (let-node var
			 (rco-expr val)
			 atom)
	       bindings)))
    ((prim-node)
     (let* ((name (gensym "TMP."))
	    (var (var-node name)))
       (values var `((,var . ,(rco-expr expr))))))))


(defun remove-complex-operands (program)
  (program (program-info program)
	   (rco-expr (program-body program))))
