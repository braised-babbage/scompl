(in-package #:scompl)

(defun explicate-tail (tail-expr)
  "Make control explicit in the given TAIL-EXPR."
  (trivia:match tail-expr
    ((var-node) (return-node tail-expr))
    ((int-node) (return-node tail-expr))
    ((let-node :var var :val val :body body)
     (explicate-assign var val
                       (explicate-tail body)))
    ((prim-node) (return-node tail-expr))
    (_ (error "Unexpected expression ~A" tail-expr))))

(defun explicate-assign (x v cont)
  "Generate an assignment X := V, followed by the continuation CONT."
  (trivia:match v
    ((var-node) (seq-node (assign-node x v) cont))
    ((int-node) (seq-node (assign-node x v) cont))
    ;; no complex operands => nothing to do here
    ((prim-node) (seq-node (assign-node x v) cont))
    ((let-node :var y :val w :body body)
     ;; to use V, we must first emit Y := W
     (explicate-assign y w
                       (explicate-assign x body cont)))
    (_ (error "Unexpected expression ~A" v))))

(defun explicate-control (program)
  "Lower PROGRAM to have explicit control flow.

This returns a CVar program."
  (let ((tail (explicate-tail (program-body program))))    
    (cprogram (program-info program)
              (list (cblock ':start tail)))))
