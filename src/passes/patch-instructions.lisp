(in-package #:x86-64)


(defun patch-instructions (program)
  (labels ((patch-block (blk)
	     (basic-block (basic-block-label blk)
			  (loop :for instr :in (basic-block-instrs blk)
				:append (patch-instruction instr))
			  (basic-block-globalp blk)))
	   (patch-instruction (instr)
	     (trivia:match instr
	       ((instr (op op) (arg1 arg1) (arg2 arg2))
		(cond ((and (deref-p arg1) (deref-p arg2))
		       (list (movq arg1 (reg :rax))
			     (instr op (reg :rax) arg2)))
		      (t (list instr))))
	       (_ (list instr)))))
    (program (program-info program)
	     (mapcar #'patch-block (program-blocks program)))))
