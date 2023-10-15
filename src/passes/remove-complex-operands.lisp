(in-package #:scompl)

(defun get-equivalent-atom (expr)
  "Get an atom that is equivalent to EXPR.

This returns two values: the atom and an alist of bindings needed for the equivalence."
  (trivia:match expr
    ((var-node) expr)
    ((int-node) expr)
    ((let-node :var var :val val :body body)
     (multiple-value-bind (atom bindings)
	 (get-equivalent-atom body)
       (values (let-node var (rco-expr val) atom)
	       bindings)))
    ((prim-node)
     (let* ((name (make-unique-name))
	    (var (var-node name)))
       (values var `((,var . ,(rco-expr expr))))))))

(defun get-equivalent-atoms (exprs)
  (loop :for expr :in exprs
	:for (%atom %bindings) := (multiple-value-list (get-equivalent-atom expr))
	:collect %atom :into atoms
	:append %bindings :into bindings
	:finally (return (values atoms bindings)))  )

(defun rco-expr (expr)
  "Rewrite EXPR so that all arguments are atomic."
  (trivia:match expr
    ((var-node) expr)
    ((int-node) expr)
    ((let-node :var var :val val :body body)
     (let-node var (rco-expr val) (rco-expr body)))
    ((prim-node :op op :args args)
     (multiple-value-bind (atoms bindings)
	 (get-equivalent-atoms args)
       ;; generate LET statements for the bindings
       (reduce (lambda (binding expr)
		 (let-node (car binding) (cdr binding) expr))
	       bindings
	       :initial-value (prim-node op atoms)
	       :from-end t)))))

(defun remove-complex-operands (program)
  "Rewrite PROGRAM so that all arguments are atomic, i.e. either an integer or a variable."
  (program (program-info program)
	   (rco-expr (program-body program))))
