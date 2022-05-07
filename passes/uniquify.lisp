(in-package #:scompl)

(defun rename-variable (env var)
  (let* ((orig (var-node-name var))
	 (new (gensym (symbol-name orig))))
    (values (var-node new)
	    (extend-env env orig (var-node new)))))

;;; Lvar -> Lvar
(defun uniquify-expr (env expr)
  (trivia:match expr
    ((var-node (name x)) (env-ref env x))
    ((int-node) expr)
    ((let-node)
     (multiple-value-bind (new-var env2)
	 (rename-variable env (let-node-var expr))
       (let-node new-var
		 (uniquify-expr env (let-node-val expr))
		 (uniquify-expr env2 (let-node-body expr)))))
    ((prim-node (op op) (args args))
     (prim-node op
		(loop :for arg :in args
		      :collect (uniquify-expr env arg))))
    (_ (error "Unexpected expression ~A" expr))))

(defun uniquify (program)
  (program (program-info program)
	   (uniquify-expr (make-environment)
			  (program-body program))))
