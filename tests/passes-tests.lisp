(in-package #:scompl-tests)


(defvar *uniquify-programs*
  (list
   '(+ 5 (- 3))
   '(let ((x 1)) x)
   '(let ((x 1))
     (let ((x x))
       x))
   '(let ((x (let ((x 1)) x)))
     x)
   '(+
     (let ((x 1)) x)
     (let ((y 1))
       (let ((x y))
	 x)))))

(defun equalp-after-transform (program &key transform
					 (eval-orig #'scompl:interpret)
					 (eval-transformed #'scompl:interpret))
  (let ((orig (funcall eval-orig program))
	(tran (funcall eval-transformed (funcall transform program))))
    (equalp orig tran)))

(deftest test-uniquify-program-equivalence ()
  (dolist (source *uniquify-programs*)
    (is (equalp-after-transform (scompl:parse-program source)
				:transform #'scompl:uniquify))))

(defvar *remove-complex-operands-programs*
  (list
   '(+ (- 10 2) (- 7))
   '(let ((a (let ((a 3)) a))) a)
   '(let ((a (+ 10 2)))
     (let ((b (- a)))
       b))))

(deftest test-remove-complex-operands-program-equivalence ()
  (dolist (source (append *uniquify-programs*
			  *remove-complex-operands-programs*))
    (is (equalp-after-transform
	 (scompl:parse-program source)
	 :transform #'scompl:remove-complex-operands))))

(defvar *explicate-control-programs*
  (list
   '(let ((y (let ((x 20))
	       (let ((z 22))
		 (+ x z)))))
     y)
   '(let ((z 10))
     (+ z z))
   '(let ((x (let ((y (- 10 5))) y)))
     (+ x x))))

(deftest test-explicate-control-program-equivalence ()
  (dolist (source *explicate-control-programs*)
    (is (equalp-after-transform (scompl:parse-program source)
				:transform #'scompl:explicate-control
				:eval-transformed #'scompl:interpret-cvar))))
