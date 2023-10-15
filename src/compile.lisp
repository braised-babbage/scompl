(in-package #:scompl)

(defvar *compiler-passes*
  (list 'uniquify
        'remove-complex-operands
        'explicate-control
        'select-x86-instructions
        'x86-64:assign-homes
        'x86-64:patch-instructions
        'x86-64:wrap-in-standard-boilerplate))

(defun compile-program (program &key
                                  (passes *compiler-passes*)
                                  (dump-to-stream nil))
  (declare (optimize (debug 3)))
  (flet ((printer (program)
           (cond ((typep program 'program)
                  (print-program program dump-to-stream))
                 ((typep program 'x86-64:program)
                  (x86-64::print-program program dump-to-stream))
                 (t (print program dump-to-stream)))
           (terpri dump-to-stream)))
    (when dump-to-stream
      (format dump-to-stream ";; Input~%")
      (printer program))
    (dolist (pass passes)
      (setf program (funcall pass program))
      (when dump-to-stream
        (format dump-to-stream ";; After ~A~%" pass)
        (printer program))))
  program)
