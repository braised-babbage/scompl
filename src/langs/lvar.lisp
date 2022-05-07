(in-package #:scompl)

(defstruct (var-node (:constructor var-node (name)))
  name)
(defstruct (let-node (:constructor let-node (var val body)))
  var
  val
  body)

(defclass lvar-interpreter (lint-interpreter)
  ())

(defmethod interpret-expr ((interp lvar-interpreter) env expr)
  (declare (optimize (debug 3)))
  (trivia:match expr
    ((var-node (name x))
     (or (env-ref env x)
	 (error "Unknown variable ~A" x)))
    ((let-node)
     (let ((new-env
	     (extend-env env
			 (var-node-name (let-node-var expr))
			 (interpret-expr interp env (let-node-val expr)))))       
       (interpret-expr interp new-env (let-node-body expr))))
    (_ (call-next-method))))
