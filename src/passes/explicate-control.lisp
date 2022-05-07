(in-package #:scompl)

(defun explicate-tail (expr)
  (trivia:match expr
    ((var-node) (return-node expr))
    ((int-node) (return-node expr))
    ((let-node (var var) (val val) (body body))
     (explicate-assign var val
		       (explicate-tail body)))
    ((prim-node) (return-node expr))
    (_ (error "Unexpected expression ~A" expr))))

(defun explicate-assign (x v cont)
  (trivia:match v
    ((var-node) (seq-node (assign-node x v) cont))
    ((int-node) (seq-node (assign-node x v) cont))
    ;; no complex operands => nothing to do here
    ((prim-node) (seq-node (assign-node x v) cont))
    ((let-node (var y) (val w) (body body))
     (explicate-assign y w
		       (explicate-assign x body cont)))
    (_ (error "Unexpected expression ~A" v))))

(defun explicate-control (program)
  (let ((tail (explicate-tail (program-body program))))    
    (cprogram (program-info program)
	      (list (cblock ':start tail)))))
