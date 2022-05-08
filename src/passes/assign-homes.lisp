(in-package #:x86-64)

(defun assign-homes-in-instr (instr homes)
  (flet ((rewire (arg)
	   (trivia:match arg
	     ((var (name name))
	      (let ((new-arg (cdr (assoc name homes))))
		(unless new-arg
		  (error "Unable to resolve arg ~A" name))
		new-arg))
	     (_ arg))))    
    (trivia:match instr
      ((instr (op op) (arg1 arg1) (arg2 arg2))
       (instr op (rewire arg1) (rewire arg2)))
      (_ instr))))

(defun assign-homes-in-block (blk homes)
  (basic-block (basic-block-label blk)
	       (mapcar (lambda (instr)
			 (assign-homes-in-instr instr homes))
		       (basic-block-instrs blk))
	       (basic-block-globalp blk)))

(defun assign-homes (program)
  (let ((stack-size 8)			; saved base pointer
	(homes nil)
	(types-binding (assoc ':locals-types (program-info program))))
    (unless types-binding
      (error ":LOCAL-TYPES required in program info"))
    (loop :for (name . type) :in (scompl::environment-alist (cdr types-binding))
	  :do (push (cons name (deref :rbp (- stack-size)))
		    homes)
	      (incf stack-size 8))
    (program (acons ':stack-size stack-size
		    (program-info program))
	     (mapcar (lambda (blk)
		       (assign-homes-in-block blk homes))
		     (program-blocks program)))))
