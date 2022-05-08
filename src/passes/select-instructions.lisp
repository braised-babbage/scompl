(in-package #:scompl)

(defun select-for-atom (atom)
  (trivia:match atom
    ((var-node (name name)) (x86-64:var name))
    ((int-node (value v)) (x86-64:imm v))
    (_ (error "Expected an atom but got ~A" atom))))

(defun select-for-stmt (stmt)
  (trivia:match stmt
    ((assign-node (var v) (val val))
     (let ((sv (select-for-atom v)))       
       (trivia:match val
	 ((int-node) (list (x86-64:movq (select-for-atom val) sv)))
	 ((var-node) (list (x86-64:movq (select-for-atom val) sv)))
	 ((prim-node (op '+) (args args))
	  (destructuring-bind (s1 s2) (mapcar #'select-for-atom args)
	    (cond ((equalp s1 sv)
		   (list (x86-64:addq s2 sv)))
		  ((equalp s2 sv)
		   (list (x86-64:addq s1 sv)))
		  (t (list (x86-64:movq s1 sv)
			   (x86-64:addq s2 sv))))))
	 ((prim-node (op '-) (args (list atm1)))
	  (let ((s1 (select-for-atom atm1)))	    
	    (cond ((equalp sv s1)
		   (list (x86-64:negq sv)))
		  (t
		   (list (x86-64:movq s1 sv)
			 (x86-64:negq sv))))))
	 ((prim-node (op '-) (args args))
	  (destructuring-bind (s1 s2) (mapcar #'select-for-atom args)
	    (cond ((equalp s1 sv)
		   (list (x86-64:subq s2 sv)))
		  (t (list (x86-64:movq s1 sv)
			   (x86-64:subq s2 sv))))))
	 ((prim-node (op 'read))
	  (cons (x86-64:callq (x86-64:label :|read_int|) 0)
		(x86-64:movq (x86-64:reg :rax) sv)))
	 (_ (error "Unexpected statement ~A" stmt)))))))

(defun select-for-tail (tail)
  (trivia:match tail
    ((return-node (val v))
     (list (x86-64:movq (select-for-atom v) (x86-64:reg :rax))
	   (x86-64:jmp (x86-64:label ':conclusion))))
    ((seq-node (stmt stmt) (tail tail2))
     (append (select-for-stmt stmt)
	     (select-for-tail tail2)))
    (_ (error "Unexpected ~A" tail))))

(defun select-instructions (cprogram)
  (flet ((translate-block (cblk)
	   (x86-64:basic-block
	    (x86-64:label (cblock-label cblk))
	    (select-for-tail (cblock-tail cblk)))))
    (x86-64:program
	     (acons ':locals-types (recover-types cprogram) nil)
	     (mapcar #'translate-block (cprogram-cblocks cprogram)))))


(defun recover-types (cprogram)
  (let ((env (make-environment)))    
    (labels
	((recover (tail)
	   (trivia:match tail
	     ((return-node) nil)
	     ((seq-node (stmt (assign-node (var var) (val val)))
			(tail tail2))
	      (setf (env-ref env (var-node-name var))
		    (infer-type val))
	      (recover tail2))
	     (_ (error "Unexpected tail ~A" tail))))
	 (infer-type (expr)
	   (declare (ignore expr))
	   ;; for now, there's not much to do here...
	   ':integer))
      (loop :for cblk :in (cprogram-cblocks cprogram)
	    :do (recover (cblock-tail cblk)))
      ;; just to make things a bit nicer, make entries
      ;; appear in program order
      (setf (environment-alist env)
	    (nreverse (environment-alist env)))
      env)))
