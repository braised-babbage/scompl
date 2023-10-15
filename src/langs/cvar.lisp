(in-package #:scompl)

;;; Concrete syntax
;;; 
;;;   atm ::= int | var
;;;   exp := atm | (read) | (- atm) | (+ atm atm) | (- atm atm)
;;;   stmt := var = exp;
;;;   tail := return exp; | stmt tail
;;;   Cvar := (label: tail) ...

(defstruct (assign-node (:constructor assign-node (var val)))
  var
  val)

(defstruct (return-node (:constructor return-node (val)))
  val)

(defstruct (seq-node (:constructor seq-node (stmt tail)))
  stmt
  tail)

(defstruct (cblock (:constructor cblock (label tail)))
  label
  tail)

(defstruct (cprogram (:constructor cprogram (info cblocks)))
  info
  cblocks)


(defvar *cvart-start-label* ':start)

(defclass cvar-interpreter (lint-interpreter)
  ())

(deftype tail-node ()
  '(or return-node seq-node))

(defmethod interpret-expr ((interp cvar-interpreter) env expr)
  (trivia:match expr
    ((var-node) (env-ref env (var-node-name expr)))
    (_ (call-next-method))))

(defgeneric interpret-stmt (interp env stmt)
  (:method ((interp cvar-interpreter) env stmt)
    (declare (optimize (debug 3)))
    (trivia:match stmt
      ((assign-node (var var) (val val))
       (setf (env-ref env (var-node-name var))
	     (interpret-expr interp env val)))
      ((return-node (val val))
       (interpret-expr interp env val))
      ((seq-node (stmt stmt2) (tail tail))
       (interpret-stmt interp env stmt2)
       (interpret-stmt interp env tail))
      (_ (error "Unexpected statement ~A" stmt)))))

(defun interpret-cvar (cprogram)
  (let ((blk (find ':start
		   (cprogram-cblocks cprogram)
		   :key #'cblock-label)))
    (unless blk
      (error "Expected a CBLOCK labeled :START."))
    (interpret-stmt (make-instance 'cvar-interpreter)
		    (make-environment)
		    (cblock-tail blk))))
