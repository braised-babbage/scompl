(in-package #:scompl)

(defun make-unique-name (&optional (name ':tmp))
  "Construct a unique name with the given prefix."
  (gensym
   (concatenate 'string
		(symbol-name name)
		".")))

(defun uniquify-expr (env expr)
  "Rewrite EXPR so that all names introduced by LET statements are unique.

Here ENV is an environment mapping names to renamed variables."
  (flet ((uniquify-variable (env var)
	   (let* ((orig-name (var-node-name var))
		  (new-name (make-unique-name orig-name))
		  (new-var (var-node new-name))
		  (new-env (extend-env env orig-name new-var)))
	     (values new-env new-var))))
    (trivia:match expr
      ((var-node :name x) (env-ref env x))
      ((int-node) expr)
      ((let-node :var var :val val :body body)
       (multiple-value-bind (new-env new-var)
	   (uniquify-variable env var)
	 (let-node new-var
		   (uniquify-expr env val)
		   (uniquify-expr new-env body))))
      ((prim-node :op op :args args)
       (prim-node op
		  (loop :for arg :in args
			:collect (uniquify-expr env arg))))
      (_ (error "Unexpected expression ~A" expr)))))

(defun uniquify (program)
  "Rewrite PROGRAM so that all names introduced by LET statements are unique."
  (program (program-info program)
	   (uniquify-expr (make-environment)
			  (program-body program))))
