(in-package #:scompl)

(defstruct (int-node (:constructor int-node (value)))
  value)
(defstruct (prim-node (:constructor prim-node (op args)))
  op
  args)
(defstruct (var-node (:constructor var-node (name)))
  name)
(defstruct (let-node (:constructor let-node (var val body)))
  var
  val
  body)
(defstruct (program (:constructor program (info body)))
  info
  body)
(defstruct environment
  alist)

(defun env-ref (env key)
  (cdr
   (assoc key (environment-alist env))))

(defun extend-env (env key val)
  (make-environment :alist (acons key val (environment-alist env))))

(defun (setf env-ref) (val env key)
  (let ((binding (assoc key (environment-alist env))))
    (cond (binding (setf (cdr binding) val))
	  (t (push (cons key val) (environment-alist env))))
    val))

(defmethod print-object ((env environment) stream)
  (print-unreadable-object (env stream :type t)
    (when (environment-alist env)
      (format stream "~A: ~A"
	      (caar (environment-alist env))
	      (cdar (environment-alist env))))
    (loop :for (key . val) :in (rest (environment-alist env))
	  :do (format stream ", ~A: ~A" key val))))

(defgeneric interpret-expr (interp env expr)
  (:documentation "Interpret EXPR, relative to ENV."))

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

(defun interpret (program &key (interpreter 'lvar-interpreter))
  (interpret-expr
   (make-instance interpreter)
   (make-environment)
   (program-body program)))

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

(defun pe-lint (p)
  (labels
      ((neg (r)
	 (trivia:match r
	   ((int-node :value n) (int-node (- 0 n)))
	   (_ (prim-node '- (list r)))))
       (add (r1 r2)
	 (trivia:match (cons r1 r2)
	   ((cons (int-node :value n1) (int-node :value n2))
	    (int-node (+ n1 n2)))
	   (_ (prim-node '+ (list r1 r2)))))
       (sub (r1 r2)
	 (trivia:match (cons r1 r2)
	   ((cons (int-node :value n1) (int-node :value n2))
	    (int-node (- n1 n2)))
	   (_ (prim-node '- (list r1 r2)))))
       (expr (e)
	 (trivia:match e
	   ((int-node) e)
	   ((prim-node :op 'read) e)
	   ((prim-node :op '- :args (list e1)) (neg (expr e1)))
	   ((prim-node :op '+ :args (list e1 e2)) (add (expr e1) (expr e2)))
	   ((prim-node :op '- :args (list e1 e2)) (sub (expr e1) (expr e2)))
	   (_ (error "Unexpected expression ~A" e)))))
    (trivia:match p
      ((program :info nil :body e) (program nil (expr e))))))
