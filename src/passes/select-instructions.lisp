(in-package #:scompl)

(defun atom-to-arg (atom)
  "Translate ATOM to an x86 argument.

Note that at this stage we consider x86 augmented with variables.
These will subsequently be allocated to registers or stack."
  (trivia:match atom
    ((var-node (name name)) (x86-64:var name))
    ((int-node (value v)) (x86-64:imm v))
    (_ (error "Expected an atom but got ~A" atom))))

(defun select-x86-for-stmt (stmt)
  "Return a list of x86 instructions implementing the given STMT."
  (trivia:match stmt
    ((assign-node :var var :val val)
     (let ((dst (atom-to-arg var)))       
       (trivia:match val
         ((int-node) (list (x86-64:movq (atom-to-arg val) dst)))
         ((var-node) (list (x86-64:movq (atom-to-arg val) dst)))
         ((prim-node :op op :args args)
          (let ((lowered-args (mapcar #'atom-to-arg args)))
            (case op
              (READ
               (list (x86-64:callq (x86-64:label :_read_int) 0)
                     (x86-64:movq (x86-64:reg :rax) dst)))
              (+
               (destructuring-bind (s1 s2) lowered-args
                 (cond ((equalp s1 dst)
                        (list (x86-64:addq s2 dst)))
                       ((equalp s2 dst)
                        (list (x86-64:addq s1 dst)))
                       (t (list (x86-64:movq s1 dst)
                                (x86-64:addq s2 dst))))))
              (-
               (if (rest lowered-args)
                   ;; (- s1 s2)
                   (destructuring-bind (s1 s2) lowered-args
                     (cond ((equalp s1 dst)
                            (list (x86-64:subq s2 dst)))
                           (t (list (x86-64:movq s1 dst)
                                    (x86-64:subq s2 dst)))))
                   ;; (- s1)
                   (destructuring-bind (s1) lowered-args
                     (cond ((equalp dst s1)
                            (list (x86-64:negq dst)))
                           (t
                            (list (x86-64:movq s1 dst)
                                  (x86-64:negq dst)))))))
              (otherwise (error "Unexpected primop ~A" op))))))))
    (_ (error "Unexpected statement ~A" stmt))))

(defun select-x86-for-tail (tail)
  "Return a list of x86 instructions for the given TAIL."
  (trivia:match tail
    ((return-node :val val)
     (list (x86-64:movq (atom-to-arg val) (x86-64:reg :rax))
           (x86-64:jmp (x86-64:label ':conclusion))))
    ((seq-node :stmt stmt :tail tail2)
     (append (select-x86-for-stmt stmt)
             (select-x86-for-tail tail2)))
    (_ (error "Unexpected ~A" tail))))

(defun select-x86-instructions (cprogram)
  "Translate CPROGRAM to an equivalent x86 program."
  (flet ((translate-block (cblk)
           (x86-64:basic-block
            (x86-64:label (cblock-label cblk))
            (select-x86-for-tail (cblock-tail cblk)))))
    (x86-64:program
     (acons ':locals-types (recover-types cprogram) nil)
     (mapcar #'translate-block (cprogram-cblocks cprogram)))))

(defun recover-types (cprogram)
  "Recover type information for the given CPROGRAM.

Returns an environment mapping variable names to their types."
  (let ((env (make-environment)))    
    (labels
        ((recover (tail)
           (trivia:match tail
             ((return-node) nil)
             ((seq-node :stmt (assign-node :var var :val val)
                        :tail tail2)
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
