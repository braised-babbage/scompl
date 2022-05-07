(in-package #:scompl)

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


(defclass interpreter ()
  ())

(defgeneric interpret-expr (interp env expr)
  (:documentation "Interpret EXPR, relative to ENV."))

(defun interpret (program &key (interpreter 'lvar-interpreter))
  (interpret-expr
   (make-instance interpreter)
   (make-environment)
   (program-body program)))
