(in-package #:x86-64)

(defstruct (imm (:constructor imm (value)))
  (value nil :type integer))
(defstruct (reg (:constructor reg (name)))
  (name nil :type symbol))
;;; useful for before register allocation...
(defstruct (var (:constructor var (name)))
  (name nil :type symbol))
(defstruct (deref (:constructor deref (base offset)))
  (base nil :type symbol)
  (offset nil :type integer))

(deftype arg ()
  '(or imm reg deref var))

(defstruct (label (:constructor label (name)))
  (name nil :type symbol))

(defstruct (instr (:constructor instr (op &optional arg1 arg2)))
  (op nil :type symbol)
  (arg1 nil :type (or null arg))
  (arg2 nil :type (or null arg)))

(defmacro define-instruction (name args)
  `(defun ,name ,args
     ,@(loop :for arg :in args
	     :collect `(cond ((integerp ,arg)
			      (setf ,arg (imm ,arg)))
			     ((and (symbolp ,arg) (not (null ,arg)))
			      (setf ,arg (reg ,arg)))))
     (instr ',name ,@args)))

(define-instruction addq (src dest))
(define-instruction subq (src dest))
(define-instruction negq (x))
(define-instruction movq (src dest))
(define-instruction pushq (x))
(define-instruction popq (x))
(define-instruction retq ())

(defstruct (callq (:constructor %callq (label arity)))
  (label nil :type label)
  (arity nil :type integer))
(defun callq (label arity)
  (%callq (if (symbolp label) (label label) label) arity))
(defstruct (jmp (:constructor %jmp (label)))
  (label nil :type label))
(defun jmp (label)
  (%jmp (if (symbolp label) (label label) label)))

(defstruct (basic-block (:constructor basic-block (label instrs &optional globalp)))
  info
  (globalp nil :type boolean)
  (label nil :type label)
  (instrs nil :type list))

(defstruct (program (:constructor program (info blocks)))
  info
  (blocks nil :type list))

(defun asm-format (stream obj &optional colon-modifier at-modifier)
  (declare (ignore colon-modifier at-modifier))
  (print-assembly obj stream))

(defgeneric print-assembly (obj stream)
  (:method ((obj imm) stream)
    (format stream "$~D" (imm-value obj)))
  (:method ((obj reg) stream)
    (let ((*print-case* ':downcase))      
      (format stream "%~A" (reg-name obj))))
  (:method ((obj var) stream)
    (format stream "~A" (var-name obj)))
  (:method ((obj deref) stream)
    (let ((*print-case* ':downcase))      
      (format stream "~D(%~A)"
	      (deref-offset obj)
	      (deref-base obj))))
  (:method ((obj label) stream)
    (let ((*print-case* ':downcase))
      (format stream "~A" (label-name obj))))
  (:method ((obj instr) stream)
    (let ((*print-case* ':downcase))     
      (format stream "       ~A" (instr-op obj)))
    (when (instr-arg1 obj)
      (format stream "    ~/x86-64::asm-format/" (instr-arg1 obj))
      (when (instr-arg2 obj)
	(format stream ", ~/x86-64::asm-format/" (instr-arg2 obj))))
    (terpri stream))
  (:method ((obj callq) stream)
    (format stream "       callq ~/x86-64::asm-format/~%" (callq-label obj)))
  (:method ((obj jmp) stream)
    (format stream "       jmp ~/x86-64::asm-format/~%" (jmp-label obj)))
  (:method ((obj basic-block) stream)
    (let ((label (basic-block-label obj))
	  (*print-case* ':downcase))
      (when (basic-block-globalp obj)
	(format stream "~%      .global ")
	(print-assembly label stream)
	(terpri stream))
      (print-assembly label stream)
      (format stream ":~%")
      (map nil (lambda (i) (print-assembly i stream)) (basic-block-instrs obj))))
  (:method ((obj program) stream)
    (map nil (lambda (bl) (print-assembly bl stream)) (program-blocks obj))))

(defun print-program (program &optional (stream *standard-output*))
  (print-assembly program stream))

(defvar *example*
  (program
   nil
   (list
    (basic-block
     (label :start)
     (list 
      (movq 10 (deref :rbp -8))
      (negq (deref :rbp -8))
      (movq (deref :rbp -8) :rax)
      (addq 52 :rax)
      (jmp :conclusion)))
    (basic-block
     (label :_main)
     (list
      (pushq :rbp)
      (movq :rsp :rbp)
      (subq 16 :rsp)
      (jmp :start))
     t)
    (basic-block
     (label :conclusion)
     (list
      (addq 16 :rsp)
      (popq :rbp)
      (retq))))))

(defun assemble-and-run (program &key (delete t))
  (let ((tmpdir (uiop:temporary-directory))) 
    (uiop:with-current-directory (tmpdir)
      (uiop:with-temporary-file (:stream stream
				 :pathname asm-file
				 :directory tmpdir
				 :type "S")
	(let ((bin-file
		(make-pathname :directory (pathname-directory asm-file)
			       :name (pathname-name asm-file))))
	  (print-program program stream)
	  (finish-output stream)
	  (unwind-protect
	       (progn	       
		 (uiop:run-program
		  (list "gcc" (namestring asm-file) "-o" (namestring bin-file)))
		 (handler-case		     
		     (uiop:run-program (namestring bin-file))
		   ;; we encode the result of the program in the error code
		   (uiop/run-program::subprocess-error (e)
		     (uiop/run-program::subprocess-error-code e))))
	    (when delete	      
	      (uiop:delete-file-if-exists bin-file))))))))
