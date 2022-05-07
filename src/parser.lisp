(in-package #:scompl)

(defun parse-program (sexp)
  (labels
      ((parse-expr (x)
	 (trivia:match x
	   ((type integer) (int-node x))
	   ((type symbol) (var-node x))
	   ((list 'read) (prim-node 'read nil))
	   ((list '+ e1 e2) (prim-node '+ (list (parse-expr e1) (parse-expr e2))))
	   ((list '- e1 e2) (prim-node '- (list (parse-expr e1) (parse-expr e2))))
	   ((list '- e) (prim-node '- (list (parse-expr e))))
	   ((list 'let (list (list var val)) body)
	    (let-node (parse-expr var) (parse-expr val) (parse-expr body)))
	   (_ (error "Invalid expression ~A" x)))))    
    (program nil (parse-expr sexp))))

(defun expr-to-list (expr)
  (trivia:match expr
    ((int-node :value v) v)
    ((var-node :name v) v)
    ((let-node)
     `(let ((,(expr-to-list (let-node-var expr))
	      ,(expr-to-list (let-node-val expr))))
	,(expr-to-list (let-node-body expr))))
    ((prim-node :op op :args args) (cons op (mapcar #'expr-to-list args)))
    (_ (error "Unexpected expression ~A" expr))))

(defun print-program (p &optional stream)
  (print (expr-to-list (program-body p)) stream)
  nil)
